# Helper: get kubectl contexts
function __kubectl_contexts
    kubectl config get-contexts -o name 2>/dev/null
end

# Helper: get namespaces for a context
function __kubectl_namespaces
    set -l context (commandline -opc)[2]  # Get context from command line
    kubectl --context=$context get namespaces -o name 2>/dev/null | string replace 'namespace/' ''
end

# Helper: get pods in a namespace
function __kubectl_pods
    set -l cmd (commandline -opc)
    set -l context $cmd[2]
    set -l namespace $cmd[3]
    kubectl --context=$context -n $namespace get pods -o name 2>/dev/null | string replace 'pod/' ''
end

# First argument: context
complete -c inspect_pod_logs -n "__fish_is_nth_token 1" -a "(__kubectl_contexts)" -d "Context"

# Second argument: namespace
complete -c inspect_pod_logs -n "__fish_is_nth_token 2" -a "(__kubectl_namespaces)" -d "Namespace"

# Third argument: pod
complete -c inspect_pod_logs -n "__fish_is_nth_token 3" -a "(__kubectl_pods)" -d "Pod"
