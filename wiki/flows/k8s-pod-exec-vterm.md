---
type: flow
related_components: [kubernetes, rpc-bridge, jsonrpc-dispatcher]
---

# Flow: Interactive Pod Shell in vterm

`ecloud-k8s-pod-exec-vterm` opens a real shell inside a pod, rendered in an Emacs
vterm buffer, with I/O bridged over the WebSocket.

```
Emacs                                          Server
─────                                          ──────
ecloud-k8s-pod-exec-vterm
  ├─ ensure WebSocket connected (ecloud-ws-connect)
  ├─ create vterm buffer backed by an inert
  │   `tail -f /dev/null` subprocess
  └─ RPC k8s_pod_exec_interactive ───────────▶ _k8s_pod_exec_interactive
       (namespace, pod, rows, cols)              └─ K8sPodExecStreamer.start_exec_session
                                                     (tty=True, stdin=True,
                                                      on_output, on_close)
       ◀──── result {session_id} ─────────────    returns session_id
  store session_id (buffer-local)

  user types ──▶ k8s_pod_exec_send_input ────▶ streamer.send_input(session_id, "ls\n")
                  (session_id, data)              └─ write to exec stdin
                                                  pod stdout/stderr
                                                  └─ on_output(session_id, chunk)
                                                       └─ broadcast {type:"k8s_exec_output",
                                                                     data:{session_id,output}}
  ecloud-ws--handle-message ◀──── WS push ──────────────────────────────────────┘
    └─ ecloud-k8s-exec-hook
         └─ ecloud-k8s--on-vterm-output → vterm render

  resize ──▶ k8s_pod_exec_resize ────────────▶ streamer.resize(session_id, rows, cols)
                                                  └─ write {"Height","Width"} to channel 4
  exit  ──▶ k8s_pod_exec_stop_session ───────▶ streamer.stop_session(session_id)
                                                  └─ on_close → broadcast
                                                     {type:"k8s_exec_session_stopped"}
  ecloud-k8s-event-hook ◀──── WS push ───────────────────────────────────────────┘
    └─ mark vterm buffer closed
```

## Why the inert subprocess

Emacs `vterm` needs a live PTY subprocess to initialize libvterm. The real shell
runs in the pod, not locally, so a no-op `tail -f /dev/null` provides the PTY
while actual I/O is bridged over the WebSocket.

## See Also
- [[kubernetes]], [[websocket-events]], [[003-websocket-push-streaming]]
