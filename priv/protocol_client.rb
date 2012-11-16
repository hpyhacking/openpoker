#!/usr/bin/ruby
#
DEFINE = Hash.new
DEF_OUT =  ""
EXPORT_OUT =  ""

SAVED = {
  :NotifyGame => 'game_id',
  :NotifySeat => 'sn'
}

INTEGER = 't_integer'
STRING = 't_string'
COIN = 't_coin'
CARD = 't_card'
CARDS = 't_cards'
BYTE = 't_byte'
LIMIT = 't_limit'

TYPES = {
  :card => CARD,
  :cards => CARDS,
  :limit => LIMIT,

  :identity => STRING,
  :password => STRING,
  :name => STRING,
  :nick => STRING,
  :photo => STRING,

  :state => INTEGER,
  :game_id => INTEGER,
  :player_id => INTEGER,
  :size => INTEGER,

  :min => COIN,
  :max => COIN,
  :pot => COIN,
  :bet => COIN,
  :call => COIN,
  :raise => COIN,
  :buyin => COIN,
  :inplay => COIN,
  :balance => COIN,
  :amount => COIN,

  :b => BYTE,
  :bb => BYTE,
  :sb => BYTE,
  :sn => BYTE,
  :stage => BYTE,
  :seats => BYTE,
  :joined => BYTE, :require => BYTE, :rank => BYTE, :suit => BYTE, :high1 => BYTE,
  :high2 => BYTE,
  :error => BYTE
}

def to_n str
  str.downcase.split('_').map do |x|
    x.gsub(/\b\w/) { $&.upcase }
  end.join()
end

def parse_def file
  File.open(file).each_line do |line| 
    next unless line =~ /record\((cmd_[a-z_]*|notify_[a-z_]*),\s*\{(.*)\}/

    record = $1
    internal = false

    record_def = ""
    record_def << $2.split(',').map do |x| 
      f = x.strip
      f = (f == "game") ? "game_id" : f
      f = (f == "player") ? "player_id" : f

      internal = true if (internal == false and f == "'|'")
      internal ? nil : "#{f}: #{TYPES[f.to_sym]}"
    end.select do |x| not x.nil? end.join(", ")
    DEFINE[record.upcase] = record_def == "" ? nil : record_def << "\n"
  end
end

def parse_wr file
  File.open(file).each_line do |line| 
    next unless line =~ /^\-define\((CMD_[A-Z_]*|NOTIFY_[A-Z_]*)\,\s*(\d+)\)\.$/

    record = $1
    record_id = $2

    DEF_OUT << "class App.#{to_n record} extends App.Protocol\n"
    if DEFINE[record.upcase]
      DEF_OUT << "  @reg '#{to_n record}', #{record_id}, #{DEFINE[record.upcase]}"
      if SAVED.has_key? to_n(record).to_sym
        DEF_OUT << "  @received_save '#{SAVED[to_n(record).to_sym]}'\n\n"
      elsif 
        DEF_OUT << "\n"
      end
    else
      DEF_OUT << "  @reg '#{to_n record}', #{record_id}\n\n"
    end
      
    EXPORT_OUT << "App.Protocol.join App.#{to_n record}\n"
  end
end

parse_def "include/genesis_protocol.hrl"
parse_def "include/genesis_notify.hrl"

parse_wr "include/genesis_protocol.hrl"
parse_wr "include/genesis_notify.hrl"

puts "# AUTO GENERATE, DONT EDIT!!!\n"
puts DEF_OUT
puts EXPORT_OUT
