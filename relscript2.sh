
release_name=${3:- "ex_banking"}
node="'${1:-"b"}@DESKTOP-GOMS8S8'"
port=${2:-8300}
release_name=${3:- "ex_banking"}
folder=${4:- "./prod2/ex_banking/bin/ex_banking"}
echo "Shell : Using node: $node on port: $port for release: $release_name"
rebar3 as prod release -o prod2 
RELX_REPLACE_OS_VARS=true NODE_NAME=${node} XX=33 PORT=${port} $folder console