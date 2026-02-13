# Helper function to ask about kubectl help
function __ask_for_kubectl_help
    while true
        read -P "Would you like to see some kubectl helper commands? [Yy]/[Nn]: " -n 1 response
        echo # Add newline after input

        switch $response
            case Y y
                __print_kubectl_helpers
                break
            case N n
                echo "Skipping helpful information."
                break
            case '*'
                echo "Please answer y or n."
        end
    end
end
