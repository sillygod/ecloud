"""Google Cloud Scheduler client wrapper.

Provides operations for managing Cloud Scheduler jobs, including
creating, updating, pausing, resuming, and deleting scheduled tasks.
"""

import os
from dataclasses import dataclass
from datetime import datetime
from typing import Any, List, Optional

from google.cloud import scheduler_v1
from google.protobuf import duration_pb2

from config import config
from gcs_client import _get_project_from_credentials

# Fix for gRPC DNS issues on Mac/VPN
os.environ["GRPC_DNS_RESOLVER"] = "native"


@dataclass
class JobInfo:
    """Cloud Scheduler job information."""
    name: str
    description: str
    schedule: str
    timezone: str
    state: str
    target_type: str
    target_uri: str
    last_attempt_time: str
    next_run_time: str
    retry_count: int
    retry_config: dict
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "description": self.description,
            "schedule": self.schedule,
            "timezone": self.timezone,
            "state": self.state,
            "targetType": self.target_type,
            "targetUri": self.target_uri,
            "lastAttemptTime": self.last_attempt_time,
            "nextRunTime": self.next_run_time,
            "retryCount": self.retry_count,
            "retryConfig": self.retry_config,
        }


class CloudSchedulerClient:
    """Client for Google Cloud Scheduler operations."""
    
    def __init__(self, project: str | None = None):
        """Initialize Cloud Scheduler client.
        
        Args:
            project: GCP project ID. If None, uses default from credentials.
        """
        self._project = project or config.gcs_project or _get_project_from_credentials()
        self._client = scheduler_v1.CloudSchedulerClient()
    
    def list_locations(self) -> List[str]:
        """List available Cloud Scheduler locations.
        
        Returns:
            List of location names.
            
        Note:
            Cloud Scheduler is available in most Cloud Run regions.
        """
        # Cloud Scheduler locations (subset of Cloud Run regions)
        locations = [
            # Americas
            "us-central1",
            "us-east1",
            "us-east4",
            "us-west1",
            "us-west2",
            "us-west3",
            "us-west4",
            "northamerica-northeast1",
            "southamerica-east1",
            # Europe
            "europe-central2",
            "europe-north1",
            "europe-west1",
            "europe-west2",
            "europe-west3",
            "europe-west4",
            "europe-west6",
            # Asia Pacific
            "asia-east1",
            "asia-east2",
            "asia-northeast1",
            "asia-northeast2",
            "asia-northeast3",
            "asia-south1",
            "asia-southeast1",
            "asia-southeast2",
            "australia-southeast1",
        ]
        
        return sorted(locations)
    
    def list_jobs(self, location: str = "us-central1") -> List[JobInfo]:
        """List Cloud Scheduler jobs in a location.
        
        Args:
            location: GCP location (e.g., 'us-central1').
            
        Returns:
            List of JobInfo objects.
        """
        parent = f"projects/{self._project}/locations/{location}"
        
        try:
            jobs = self._client.list_jobs(parent=parent)
            result = []
            
            for job in jobs:
                # Extract job name (last part of full name)
                job_name = job.name.split("/")[-1]
                
                # Get state
                state = job.state.name if job.state else "UNKNOWN"
                
                # Determine target type and URI
                target_type = "UNKNOWN"
                target_uri = ""
                
                if job.http_target and job.http_target.uri:
                    target_type = "HTTP"
                    target_uri = job.http_target.uri
                elif job.pubsub_target and job.pubsub_target.topic_name:
                    target_type = "PUBSUB"
                    target_uri = job.pubsub_target.topic_name
                elif job.app_engine_http_target:
                    target_type = "APP_ENGINE"
                    target_uri = job.app_engine_http_target.relative_uri or "/"
                
                # Get timestamps - with safe error handling
                last_attempt = ""
                try:
                    if hasattr(job, 'status') and job.status:
                        if hasattr(job.status, 'last_attempt_time') and job.status.last_attempt_time:
                            last_attempt = job.status.last_attempt_time.isoformat()
                except Exception:
                    # Ignore timestamp errors
                    pass
                
                next_run = ""
                try:
                    if hasattr(job, 'schedule_time') and job.schedule_time:
                        next_run = job.schedule_time.isoformat()
                except Exception:
                    # Ignore timestamp errors
                    pass
                
                # Get retry config
                retry_config = {}
                if job.retry_config:
                    retry_config = {
                        "retryCount": job.retry_config.retry_count,
                        "maxRetryDuration": str(job.retry_config.max_retry_duration.seconds) + "s" if job.retry_config.max_retry_duration else "0s",
                        "minBackoffDuration": str(job.retry_config.min_backoff_duration.seconds) + "s" if job.retry_config.min_backoff_duration else "0s",
                        "maxBackoffDuration": str(job.retry_config.max_backoff_duration.seconds) + "s" if job.retry_config.max_backoff_duration else "0s",
                        "maxDoublings": job.retry_config.max_doublings,
                    }
                
                result.append(JobInfo(
                    name=job_name,
                    description=job.description or "",
                    schedule=job.schedule or "",
                    timezone=job.time_zone or "UTC",
                    state=state,
                    target_type=target_type,
                    target_uri=target_uri,
                    last_attempt_time=last_attempt,
                    next_run_time=next_run,
                    retry_count=job.retry_config.retry_count if job.retry_config else 0,
                    retry_config=retry_config,
                ))
            
            return result
        
        except Exception as e:
            import traceback
            error_details = traceback.format_exc()
            raise RuntimeError(f"Failed to list Cloud Scheduler jobs in '{location}': {e}\n\nDetails:\n{error_details}")
    
    def get_job(self, name: str, location: str = "us-central1") -> JobInfo:
        """Get details of a specific Cloud Scheduler job.
        
        Args:
            name: Job name.
            location: GCP location.
            
        Returns:
            JobInfo object.
        """
        job_path = f"projects/{self._project}/locations/{location}/jobs/{name}"
        
        try:
            job = self._client.get_job(name=job_path)
            
            # Extract job name
            job_name = job.name.split("/")[-1]
            
            # Get state
            state = job.state.name if job.state else "UNKNOWN"
            
            # Determine target type and URI
            target_type = "UNKNOWN"
            target_uri = ""
            
            if job.http_target and job.http_target.uri:
                target_type = "HTTP"
                target_uri = job.http_target.uri
            elif job.pubsub_target and job.pubsub_target.topic_name:
                target_type = "PUBSUB"
                target_uri = job.pubsub_target.topic_name
            elif job.app_engine_http_target:
                target_type = "APP_ENGINE"
                target_uri = job.app_engine_http_target.relative_uri or "/"
            
            # Get timestamps - with safe error handling
            last_attempt = ""
            try:
                if hasattr(job, 'status') and job.status:
                    if hasattr(job.status, 'last_attempt_time') and job.status.last_attempt_time:
                        last_attempt = job.status.last_attempt_time.isoformat()
            except Exception:
                # Ignore timestamp errors
                pass
            
            next_run = ""
            try:
                if hasattr(job, 'schedule_time') and job.schedule_time:
                    next_run = job.schedule_time.isoformat()
            except Exception:
                # Ignore timestamp errors
                pass
            
            # Get retry config
            retry_config = {}
            if job.retry_config:
                retry_config = {
                    "retryCount": job.retry_config.retry_count,
                    "maxRetryDuration": str(job.retry_config.max_retry_duration.seconds) + "s" if job.retry_config.max_retry_duration else "0s",
                    "minBackoffDuration": str(job.retry_config.min_backoff_duration.seconds) + "s" if job.retry_config.min_backoff_duration else "0s",
                    "maxBackoffDuration": str(job.retry_config.max_backoff_duration.seconds) + "s" if job.retry_config.max_backoff_duration else "0s",
                    "maxDoublings": job.retry_config.max_doublings,
                }
            
            return JobInfo(
                name=job_name,
                description=job.description or "",
                schedule=job.schedule or "",
                timezone=job.time_zone or "UTC",
                state=state,
                target_type=target_type,
                target_uri=target_uri,
                last_attempt_time=last_attempt,
                next_run_time=next_run,
                retry_count=job.retry_config.retry_count if job.retry_config else 0,
                retry_config=retry_config,
            )
        
        except Exception as e:
            raise RuntimeError(f"Failed to get Cloud Scheduler job '{name}': {e}")
    
    def create_http_job(
        self,
        name: str,
        schedule: str,
        uri: str,
        location: str = "us-central1",
        description: str = "",
        timezone: str = "UTC",
        http_method: str = "POST",
        headers: dict[str, str] | None = None,
        body: str = "",
        retry_count: int = 3,
    ) -> JobInfo:
        """Create a new HTTP Cloud Scheduler job.
        
        Args:
            name: Job name.
            schedule: Cron schedule (e.g., '0 9 * * *').
            uri: Target HTTP URI.
            location: GCP location.
            description: Job description.
            timezone: Timezone (e.g., 'America/New_York').
            http_method: HTTP method (GET, POST, PUT, DELETE, PATCH).
            headers: HTTP headers.
            body: HTTP request body.
            retry_count: Number of retry attempts.
            
        Returns:
            JobInfo object for the created job.
        """
        parent = f"projects/{self._project}/locations/{location}"
        job_path = f"{parent}/jobs/{name}"
        
        # Build HTTP target
        http_target = scheduler_v1.HttpTarget(
            uri=uri,
            http_method=http_method,
        )
        
        if headers:
            http_target.headers = headers
        
        if body:
            http_target.body = body.encode('utf-8')
        
        # Build retry config
        retry_config = scheduler_v1.RetryConfig(
            retry_count=retry_count,
            max_retry_duration=duration_pb2.Duration(seconds=600),  # 10 minutes
            min_backoff_duration=duration_pb2.Duration(seconds=5),
            max_backoff_duration=duration_pb2.Duration(seconds=3600),  # 1 hour
            max_doublings=5,
        )
        
        # Build job
        job = scheduler_v1.Job(
            name=job_path,
            description=description,
            schedule=schedule,
            time_zone=timezone,
            http_target=http_target,
            retry_config=retry_config,
        )
        
        try:
            created_job = self._client.create_job(parent=parent, job=job)
            return self.get_job(name, location)
        
        except Exception as e:
            raise RuntimeError(f"Failed to create Cloud Scheduler job '{name}': {e}")
    
    def create_pubsub_job(
        self,
        name: str,
        schedule: str,
        topic: str,
        location: str = "us-central1",
        description: str = "",
        timezone: str = "UTC",
        data: str = "",
        attributes: dict[str, str] | None = None,
        retry_count: int = 3,
    ) -> JobInfo:
        """Create a new Pub/Sub Cloud Scheduler job.
        
        Args:
            name: Job name.
            schedule: Cron schedule (e.g., '0 9 * * *').
            topic: Pub/Sub topic name (e.g., 'projects/PROJECT/topics/TOPIC').
            location: GCP location.
            description: Job description.
            timezone: Timezone.
            data: Message data.
            attributes: Message attributes.
            retry_count: Number of retry attempts.
            
        Returns:
            JobInfo object for the created job.
        """
        parent = f"projects/{self._project}/locations/{location}"
        job_path = f"{parent}/jobs/{name}"
        
        # Build Pub/Sub target
        pubsub_target = scheduler_v1.PubsubTarget(
            topic_name=topic,
        )
        
        if data:
            pubsub_target.data = data.encode('utf-8')
        
        if attributes:
            pubsub_target.attributes = attributes
        
        # Build retry config
        retry_config = scheduler_v1.RetryConfig(
            retry_count=retry_count,
            max_retry_duration=duration_pb2.Duration(seconds=600),
            min_backoff_duration=duration_pb2.Duration(seconds=5),
            max_backoff_duration=duration_pb2.Duration(seconds=3600),
            max_doublings=5,
        )
        
        # Build job
        job = scheduler_v1.Job(
            name=job_path,
            description=description,
            schedule=schedule,
            time_zone=timezone,
            pubsub_target=pubsub_target,
            retry_config=retry_config,
        )
        
        try:
            created_job = self._client.create_job(parent=parent, job=job)
            return self.get_job(name, location)
        
        except Exception as e:
            raise RuntimeError(f"Failed to create Cloud Scheduler job '{name}': {e}")
    
    def update_job(
        self,
        name: str,
        location: str = "us-central1",
        schedule: str | None = None,
        description: str | None = None,
        timezone: str | None = None,
    ) -> JobInfo:
        """Update a Cloud Scheduler job.
        
        Args:
            name: Job name.
            location: GCP location.
            schedule: New cron schedule (optional).
            description: New description (optional).
            timezone: New timezone (optional).
            
        Returns:
            JobInfo object for the updated job.
        """
        job_path = f"projects/{self._project}/locations/{location}/jobs/{name}"
        
        try:
            # Get existing job
            job = self._client.get_job(name=job_path)
            
            # Update fields
            if schedule is not None:
                job.schedule = schedule
            if description is not None:
                job.description = description
            if timezone is not None:
                job.time_zone = timezone
            
            # Update job
            updated_job = self._client.update_job(job=job)
            return self.get_job(name, location)
        
        except Exception as e:
            raise RuntimeError(f"Failed to update Cloud Scheduler job '{name}': {e}")
    
    def pause_job(self, name: str, location: str = "us-central1") -> bool:
        """Pause a Cloud Scheduler job.
        
        Args:
            name: Job name.
            location: GCP location.
            
        Returns:
            True if successful.
        """
        job_path = f"projects/{self._project}/locations/{location}/jobs/{name}"
        
        try:
            self._client.pause_job(name=job_path)
            return True
        
        except Exception as e:
            raise RuntimeError(f"Failed to pause Cloud Scheduler job '{name}': {e}")
    
    def resume_job(self, name: str, location: str = "us-central1") -> bool:
        """Resume a paused Cloud Scheduler job.
        
        Args:
            name: Job name.
            location: GCP location.
            
        Returns:
            True if successful.
        """
        job_path = f"projects/{self._project}/locations/{location}/jobs/{name}"
        
        try:
            self._client.resume_job(name=job_path)
            return True
        
        except Exception as e:
            raise RuntimeError(f"Failed to resume Cloud Scheduler job '{name}': {e}")
    
    def run_job(self, name: str, location: str = "us-central1") -> bool:
        """Manually trigger a Cloud Scheduler job.
        
        Args:
            name: Job name.
            location: GCP location.
            
        Returns:
            True if successful.
        """
        job_path = f"projects/{self._project}/locations/{location}/jobs/{name}"
        
        try:
            self._client.run_job(name=job_path)
            return True
        
        except Exception as e:
            raise RuntimeError(f"Failed to run Cloud Scheduler job '{name}': {e}")
    
    def delete_job(self, name: str, location: str = "us-central1") -> bool:
        """Delete a Cloud Scheduler job.
        
        Args:
            name: Job name.
            location: GCP location.
            
        Returns:
            True if successful.
        """
        job_path = f"projects/{self._project}/locations/{location}/jobs/{name}"
        
        try:
            self._client.delete_job(name=job_path)
            return True
        
        except Exception as e:
            raise RuntimeError(f"Failed to delete Cloud Scheduler job '{name}': {e}")


# Singleton instance
_client_instance: CloudSchedulerClient | None = None


def get_cloud_scheduler_client(project: str | None = None) -> CloudSchedulerClient:
    """Get or create the Cloud Scheduler client singleton.
    
    Args:
        project: GCP project ID. If None, uses default from credentials.
        
    Returns:
        CloudSchedulerClient instance.
    """
    global _client_instance
    if _client_instance is None:
        _client_instance = CloudSchedulerClient(project)
    return _client_instance


def reset_cloud_scheduler_client():
    """Reset the Cloud Scheduler client singleton.
    
    Useful for testing or when credentials change.
    """
    global _client_instance
    _client_instance = None
