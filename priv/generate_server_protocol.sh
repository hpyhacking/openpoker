sed -i '.bak' '/AUTO/,$'d ./src/protocol.erl
./priv/protocol.rb >> ./src/protocol.erl
