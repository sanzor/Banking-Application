node="${1:- "a"}@DESKTOP-GOMS8S8"
port=${2:-8300}
echo "Using node: $node on port: $port for release: $release_name"
config=${1:-"a"}
PORT=${port} rebar3 shell --sname ${node} --config $config