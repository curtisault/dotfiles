# Kubernetes log inspection with lnav
# Usage: inspect_pod_logs <context> <namespace> <pod>
function inspect_pod_logs
    if not command -v lnav > /dev/null
        echo "lnav not found. Install lnav and try again."
        return 1
    end

    if test (count $argv) -lt 3
        echo "Usage: inspect_pod_logs <context> <namespace> <pod>"
        __ask_for_kubectl_help
        return 1
    end

    set context $argv[1]
    set namespace $argv[2]
    set pod $argv[3]

    kubectl --context $context --namespace $namespace logs -f $pod | lnav
end
