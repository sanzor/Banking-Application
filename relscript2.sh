
release_name=${3:- "ex_banking"}
node="${1:- "b"}@DESKTOP-GOMS8S8"
port=${2:-8200}
release_name=${3:- "ex_banking"}
folder=${4:- "./prod1/ex_banking/bin/ex_banking"}
echo "Using node: $node on port: $port for release: $release_name"
# rebar3 release as prod1 -o prod2
NODE_NAME=${node}  PORT=${port} $folder console