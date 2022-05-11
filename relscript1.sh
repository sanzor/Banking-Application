
release_name=${3:- "ex_banking"}
node="'${1:-"a"}@DESKTOP-GOMS8S8'"
port=${2:-8300}
release_name=${3:- "ex_banking"}
folder=${4:- "./prod1/ex_banking/bin/ex_banking"}
echo "Using node: $node on port: $port for release: $release_name"
rebar3 as prod release -o prod1
echo "Node name passing is:"
echo "${node}"
echo "${port}"
 NODE_NAME=${node}  PORT=${port} $folder console 