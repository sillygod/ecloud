"""Structured error handling for ECloud backend.

Provides consistent error response format across all operations.
"""

from dataclasses import dataclass
from typing import Any


@dataclass
class ErrorDetails:
    """Additional error context and suggestions."""
    context: dict[str, Any] | None = None
    suggestion: str | None = None
    
    def to_dict(self) -> dict[str, Any]:
        result = {}
        if self.context:
            result["context"] = self.context
        if self.suggestion:
            result["suggestion"] = self.suggestion
        return result


@dataclass
class StructuredError:
    """Structured error response format."""
    type: str
    message: str
    details: ErrorDetails | None = None
    
    def to_dict(self) -> dict[str, Any]:
        result = {
            "type": self.type,
            "message": self.message,
        }
        if self.details:
            result["details"] = self.details.to_dict()
        return result
    
    def to_exception(self) -> Exception:
        """Convert to a standard Exception with formatted message."""
        msg = f"{self.type}: {self.message}"
        if self.details and self.details.suggestion:
            msg += f"\nSuggestion: {self.details.suggestion}"
        return Exception(msg)


# --- Error Type Constants ---

# Helm-specific errors
HELM_NOT_INITIALIZED = "HelmNotInitialized"
HELM_RELEASE_NOT_FOUND = "HelmReleaseNotFound"
HELM_RELEASE_ALREADY_EXISTS = "HelmReleaseAlreadyExists"
HELM_CHART_NOT_FOUND = "HelmChartNotFound"
HELM_NAMESPACE_NOT_FOUND = "HelmNamespaceNotFound"
HELM_REVISION_NOT_FOUND = "HelmRevisionNotFound"
HELM_REPOSITORY_ALREADY_EXISTS = "HelmRepositoryAlreadyExists"
HELM_REPOSITORY_NOT_FOUND = "HelmRepositoryNotFound"
HELM_INVALID_URL = "HelmInvalidUrl"
HELM_OPERATION_FAILED = "HelmOperationFailed"
HELM_CLI_NOT_FOUND = "HelmCliNotFound"

# Authentication errors
AUTH_SERVICE_ACCOUNT_NOT_SET = "AuthServiceAccountNotSet"
AUTH_SERVICE_ACCOUNT_FILE_NOT_FOUND = "AuthServiceAccountFileNotFound"
AUTH_INVALID_CREDENTIALS = "AuthInvalidCredentials"

# Kubernetes errors
K8S_NOT_CONNECTED = "K8sNotConnected"
K8S_CONNECTION_FAILED = "K8sConnectionFailed"
K8S_CLUSTER_UNREACHABLE = "K8sClusterUnreachable"

# General errors
INVALID_PARAMETER = "InvalidParameter"
OPERATION_TIMEOUT = "OperationTimeout"
INTERNAL_ERROR = "InternalError"


# --- Error Factory Functions ---

def helm_not_initialized_error() -> StructuredError:
    """Create error for Helm client not initialized."""
    return StructuredError(
        type=HELM_NOT_INITIALIZED,
        message="Helm client not initialized",
        details=ErrorDetails(
            suggestion="Please connect to a Kubernetes cluster first"
        )
    )


def helm_release_not_found_error(release_name: str, namespace: str) -> StructuredError:
    """Create error for release not found."""
    return StructuredError(
        type=HELM_RELEASE_NOT_FOUND,
        message=f"Release '{release_name}' not found in namespace '{namespace}'",
        details=ErrorDetails(
            context={"release_name": release_name, "namespace": namespace},
            suggestion="Check the release name and namespace, or list all releases to verify"
        )
    )


def helm_release_already_exists_error(release_name: str, namespace: str) -> StructuredError:
    """Create error for release already exists."""
    return StructuredError(
        type=HELM_RELEASE_ALREADY_EXISTS,
        message=f"Release '{release_name}' already exists in namespace '{namespace}'",
        details=ErrorDetails(
            context={"release_name": release_name, "namespace": namespace},
            suggestion="Use a different release name or upgrade the existing release"
        )
    )


def helm_chart_not_found_error(chart_ref: str) -> StructuredError:
    """Create error for chart not found."""
    return StructuredError(
        type=HELM_CHART_NOT_FOUND,
        message=f"Chart '{chart_ref}' not found",
        details=ErrorDetails(
            context={"chart_ref": chart_ref},
            suggestion="Ensure the chart reference is correct and the repository is added"
        )
    )


def helm_namespace_not_found_error(namespace: str) -> StructuredError:
    """Create error for namespace not found."""
    return StructuredError(
        type=HELM_NAMESPACE_NOT_FOUND,
        message=f"Namespace '{namespace}' does not exist",
        details=ErrorDetails(
            context={"namespace": namespace},
            suggestion="Set create_namespace=True to create it automatically, or create the namespace first"
        )
    )


def helm_revision_not_found_error(release_name: str, revision: int) -> StructuredError:
    """Create error for revision not found."""
    return StructuredError(
        type=HELM_REVISION_NOT_FOUND,
        message=f"Revision {revision} not found for release '{release_name}'",
        details=ErrorDetails(
            context={"release_name": release_name, "revision": revision},
            suggestion="Check the revision history to see available revisions"
        )
    )


def helm_repository_already_exists_error(name: str) -> StructuredError:
    """Create error for repository already exists."""
    return StructuredError(
        type=HELM_REPOSITORY_ALREADY_EXISTS,
        message=f"Repository '{name}' already exists",
        details=ErrorDetails(
            context={"repository_name": name},
            suggestion="Use a different name or remove the existing repository first"
        )
    )


def helm_repository_not_found_error(name: str) -> StructuredError:
    """Create error for repository not found."""
    return StructuredError(
        type=HELM_REPOSITORY_NOT_FOUND,
        message=f"Repository '{name}' not found",
        details=ErrorDetails(
            context={"repository_name": name},
            suggestion="Check the repository name or list all repositories"
        )
    )


def helm_invalid_url_error(url: str) -> StructuredError:
    """Create error for invalid repository URL."""
    return StructuredError(
        type=HELM_INVALID_URL,
        message=f"Invalid repository URL: {url}",
        details=ErrorDetails(
            context={"url": url},
            suggestion="Ensure the URL is correct and accessible"
        )
    )


def helm_operation_failed_error(operation: str, reason: str) -> StructuredError:
    """Create error for general Helm operation failure."""
    return StructuredError(
        type=HELM_OPERATION_FAILED,
        message=f"Helm {operation} operation failed: {reason}",
        details=ErrorDetails(
            context={"operation": operation},
            suggestion="Check the error details and ensure all parameters are correct"
        )
    )


def helm_cli_not_found_error() -> StructuredError:
    """Create error for Helm CLI not found."""
    return StructuredError(
        type=HELM_CLI_NOT_FOUND,
        message="Helm CLI not found in system PATH",
        details=ErrorDetails(
            suggestion="Please install Helm CLI: https://helm.sh/docs/intro/install/"
        )
    )


def auth_service_account_not_set_error() -> StructuredError:
    """Create error for Service Account not set."""
    return StructuredError(
        type=AUTH_SERVICE_ACCOUNT_NOT_SET,
        message="GOOGLE_APPLICATION_CREDENTIALS environment variable not set",
        details=ErrorDetails(
            suggestion="Set GOOGLE_APPLICATION_CREDENTIALS to the path of your Service Account JSON file"
        )
    )


def auth_service_account_file_not_found_error(path: str) -> StructuredError:
    """Create error for Service Account file not found."""
    return StructuredError(
        type=AUTH_SERVICE_ACCOUNT_FILE_NOT_FOUND,
        message=f"Service Account file not found: {path}",
        details=ErrorDetails(
            context={"path": path},
            suggestion="Ensure the file exists and the path is correct"
        )
    )


def k8s_not_connected_error() -> StructuredError:
    """Create error for Kubernetes not connected."""
    return StructuredError(
        type=K8S_NOT_CONNECTED,
        message="Not connected to a Kubernetes cluster",
        details=ErrorDetails(
            suggestion="Connect to a cluster first using k8s_connect"
        )
    )


def k8s_cluster_unreachable_error(cluster: str) -> StructuredError:
    """Create error for cluster unreachable."""
    return StructuredError(
        type=K8S_CLUSTER_UNREACHABLE,
        message=f"Kubernetes cluster '{cluster}' is unreachable",
        details=ErrorDetails(
            context={"cluster": cluster},
            suggestion="Check cluster status and network connectivity"
        )
    )


def invalid_parameter_error(param_name: str, reason: str) -> StructuredError:
    """Create error for invalid parameter."""
    return StructuredError(
        type=INVALID_PARAMETER,
        message=f"Invalid parameter '{param_name}': {reason}",
        details=ErrorDetails(
            context={"parameter": param_name},
            suggestion="Check the parameter value and format"
        )
    )


# --- Error Classification Helper ---

def classify_helm_error(error_msg: str) -> str:
    """Classify a Helm error message into an error type.
    
    Args:
        error_msg: The error message from Helm operation
        
    Returns:
        Error type constant
    """
    error_lower = error_msg.lower()
    
    if "already exists" in error_lower:
        return HELM_RELEASE_ALREADY_EXISTS
    elif "not found" in error_lower:
        if "release" in error_lower:
            return HELM_RELEASE_NOT_FOUND
        elif "chart" in error_lower or "no chart" in error_lower:
            return HELM_CHART_NOT_FOUND
        elif "revision" in error_lower:
            return HELM_REVISION_NOT_FOUND
        elif "repository" in error_lower or "repo" in error_lower:
            return HELM_REPOSITORY_NOT_FOUND
        else:
            return HELM_OPERATION_FAILED
    elif "namespace" in error_lower and ("does not exist" in error_lower or "not found" in error_lower):
        return HELM_NAMESPACE_NOT_FOUND
    elif "invalid" in error_lower or "malformed" in error_lower:
        return HELM_INVALID_URL
    elif "helm" in error_lower and "not found" in error_lower:
        return HELM_CLI_NOT_FOUND
    else:
        return HELM_OPERATION_FAILED


def create_helm_error_from_exception(operation: str, exception: Exception) -> StructuredError:
    """Create a structured error from a Helm exception.
    
    Args:
        operation: The operation that failed (e.g., "install", "upgrade")
        exception: The exception that was raised
        
    Returns:
        StructuredError with appropriate type and details
    """
    error_msg = str(exception)
    error_type = classify_helm_error(error_msg)
    
    return StructuredError(
        type=error_type,
        message=error_msg,
        details=ErrorDetails(
            context={"operation": operation},
            suggestion="Check the error message for details and verify all parameters"
        )
    )
