
release_name=${3:- "ex_banking_dev-0.1.0"}
node="${1:- "b"}@DESKTOP-GOMS8S8"
port=${2:-8300}
release_name=${3:- "ex_banking_dev-0.1.0"}
folder=${4:- "./ex_banking_dev/bin/ex_banking_dev-0.1.0"}
echo "Using node: $node on port: $port for release: $release_name"
NODE_NAME=${node}  PORT=${port} $folder console