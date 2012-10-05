sed -i '/AUTO\ GENERATE/,$d' src/protocol.erl
./priv/protocol.rb >> src/protocol.erl
