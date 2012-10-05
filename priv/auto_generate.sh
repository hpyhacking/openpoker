sed -i '/AUTO\ GENERATE/,$d' src/protocol.erl
./priv/protocol.rb >> src/protocol.erl
sed -i '/AUTO\ GENERATE/,$d' ../genesis4html5/assets/javascripts/app/models/index.coffee
./priv/protocol_client.rb >> ../genesis4html5/assets/javascripts/app/models/index.coffee
