sed -i '.bak' '/AUTO/,$'d ../genesis4html5/assets/javascripts/app/models/index.coffee
./priv/protocol_client.rb >> ../genesis4html5/assets/javascripts/app/models/index.coffee
rm ../genesis4html5/assets/javascripts/app/models/index.coffee.bak
