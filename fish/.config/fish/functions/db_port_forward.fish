# Database port forwarding
# Usage: db_port_forward <context> <resource> [port]
function db_port_forward
    if test (count $argv) -lt 2
        echo "Usage: db_port_forward <context> <resource> [port]"
        __ask_for_kubectl_help
        return 1
    end

    set context $argv[1]
    set resource $argv[2]
    set port $argv[3]
    
    # Default port to 15433 if not provided
    if test -z "$port"
        set port 15433
    end

    kubectl --context $context -n db-proxy port-forward $resource $port:5432
end
