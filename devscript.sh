node="${1:- "a"}@DESKTOP-GOMS8S8"
port=${2:-8300}
echo "Using node: $node on port: $port for release: $release_name"
PORT=${port} rebar3 shell --sname ${node}