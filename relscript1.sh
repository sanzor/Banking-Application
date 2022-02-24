
release_name=${3:- "ex_banking_dev-0.1.0"}
node="${1:- "a"}@DESKTOP-GOMS8S8"
port=${2:-8300}
release_name=${3:- "ex_banking_dev-0.1.0"}
folder=${4:- "./prod1/ex_banking_dev/bin/ex_banking_dev-0.1.0"}
echo "Using node: $node on port: $port for release: $release_name"
rebar3 release as prod1 
NODE_NAME=${node}  PORT=${port} $folder console