# Print kubectl helper commands
function __print_kubectl_helpers
    echo "Get Namespaces: kubectl get namespaces"
    echo "Get Pods: kubectl get pods -n <namespace>"
    echo "Get Services: kubectl get services -n <namespace>"
end
