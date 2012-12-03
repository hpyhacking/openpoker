sed -i '.bak' '/AUTO/,$'d ../game.kinseng/assets/javascripts/app/models/index.coffee
./priv/protocol_client.rb >> ../game.kinseng/assets/javascripts/app/models/index.coffee
rm ../game.kinseng/assets/javascripts/app/models/index.coffee.bak
