
release_name=${3:- "ex_banking"}
node="${1:- "a"}@DESKTOP-GOMS8S8"
port=${2:-8300}
release_name=${3:- "ex_banking"}
folder=${4:- "./prod1/ex_banking/bin/ex_banking"}
echo "Shell : Using node: $node on port: $port for release: $release_name"
rebar3 release as prod1 -o prod1
RELX_REPLACE_OS_VARS=true NODE_NAME=${node} XX=33 PORT=${port} $folder console