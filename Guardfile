guard 'shell' do
  watch(/include\/(.*)\.hrl/) do |m|
    if system('rebar eunit')
      Notifier.notify("SUCCESS", :title => "Genesis")
    else
      Notifier.notify("ERROR", :title => "Genesis", :image => :failed)
    end
  end
  watch(/src\/(.*)\.erl/) do |m|
    if system('rebar eunit')
      Notifier.notify("SUCCESS", :title => "Genesis")
    else
      Notifier.notify("ERROR", :title => "Genesis", :image => :failed)
    end
  end
  watch(/test\/(.*)\.erl/) do |m|
    if system('rebar eunit')
      Notifier.notify("SUCCESS", :title => "Genesis")
    else
      Notifier.notify("ERROR", :title => "Genesis", :image => :failed)
    end
  end
end
