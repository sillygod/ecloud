#!/bin/bash
# Simple test for gRPC DNS resolvers

echo "============================================================"
echo "gRPC DNS Resolver Test (Simple)"
echo "============================================================"
echo ""

# Test 1: c-ares (default)
echo "1. Testing with c-ares (default)"
echo "============================================================"
GRPC_DNS_RESOLVER=ares python3 -c "
import os
os.environ['GRPC_DNS_RESOLVER'] = 'ares'
from google.cloud import container_v1
from google.oauth2 import service_account
import json

sa_path = os.getenv('GOOGLE_APPLICATION_CREDENTIALS')
with open(sa_path) as f:
    project_id = json.load(f)['project_id']

SCOPES = ['https://www.googleapis.com/auth/cloud-platform']
credentials = service_account.Credentials.from_service_account_file(sa_path, scopes=SCOPES)
client = container_v1.ClusterManagerClient(credentials=credentials)
parent = f'projects/{project_id}/locations/-'

try:
    response = client.list_clusters(parent=parent)
    print(f'✓ SUCCESS with c-ares: {len(response.clusters)} clusters')
except Exception as e:
    print(f'❌ FAILED with c-ares: {str(e)[:100]}...')
"

echo ""
echo "2. Testing with native (system DNS)"
echo "============================================================"
GRPC_DNS_RESOLVER=native python3 -c "
import os
os.environ['GRPC_DNS_RESOLVER'] = 'native'
from google.cloud import container_v1
from google.oauth2 import service_account
import json

sa_path = os.getenv('GOOGLE_APPLICATION_CREDENTIALS')
with open(sa_path) as f:
    project_id = json.load(f)['project_id']

SCOPES = ['https://www.googleapis.com/auth/cloud-platform']
credentials = service_account.Credentials.from_service_account_file(sa_path, scopes=SCOPES)
client = container_v1.ClusterManagerClient(credentials=credentials)
parent = f'projects/{project_id}/locations/-'

try:
    response = client.list_clusters(parent=parent)
    print(f'✓ SUCCESS with native: {len(response.clusters)} clusters')
except Exception as e:
    print(f'❌ FAILED with native: {str(e)[:100]}...')
"

echo ""
echo "============================================================"
echo "SUMMARY"
echo "============================================================"
echo "If native works but c-ares fails, the fix in main.py will help."
echo "Restart ECloud server to apply the fix:"
echo "  M-x ecloud-account-restart"
