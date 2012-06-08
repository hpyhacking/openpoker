guard :shell do
  watch(/test\/(.*)\.erl/) do |m|
    if system("rebar eunit suite=#{m[1]}")
      n "#{m[1]} is successful", 'EUnit', :success
    else
      n "#{m[1]} is error", 'EUnit', :failed
    end
  end
end

guard :shell, :all_on_start => true do
  system("rebar eunit")
end
