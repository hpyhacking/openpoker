#!/usr/bin/ruby
#
DEFINE = Hash.new
DEF_OUT =  "class TestProtocol extends Protocol\n"
DEF_OUT << "  @id = 255\n"
DEF_OUT << "  constructor: (data = {}) -> data.id = TestProtocol.id; super(data)\n"
DEF_OUT << "  Protocol.reg @id, {int: integer, str: string, byte: byte, base: base64, coin: coin}\n\n"

EXPORT_OUT =  "window.protocols =\n"
EXPORT_OUT << "  test: TestProtocol\n"

TYPES = {
  :card => "card",
  :cards => "cards",
  :limit => "limit",

  :identity => "string",
  :password => "string",
  :name => "string",
  :nick => "string",
  :photo => "string",

  :state => "integer",
  :game_id => "integer",
  :player_id => "integer",

  :min => "coin",
  :max => "coin",
  :pot => "coin",
  :bet => "coin",
  :call => "coin",
  :raise => "coin",
  :buyin => "coin",
  :inplay => "coin",
  :balance => "coin",
  :amount => "coin",

  :b => "byte",
  :bb => "byte",
  :sb => "byte",
  :sn => "byte",
  :stage => "byte",
  :seats => "byte",
  :joined => "byte",
  :require => "byte",

  :rank => "byte",
  :suit => "byte",
  :high1 => "byte",
  :high2 => "byte",
  :error => "byte"
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
    record_def << "Protocol.reg @id, {"
    record_def << $2.split(',').map do |x| 
      f = x.strip
      f = (f == "game") ? "game_id" : f
      f = (f == "player") ? "player_id" : f

      internal = true if (internal == false and f == "'|'")
      internal ? nil : "#{f}: #{TYPES[f.to_sym]}"
    end.select do |x| not x.nil? end.join(", ")
    record_def << "}\n"
    DEFINE[record.upcase] = record_def
  end
end

def parse_wr file
  File.open(file).each_line do |line| 
    next unless line =~ /^\-define\((CMD_[A-Z_]*|NOTIFY_[A-Z_]*)\,\s*(\d+)\)\.$/

    record = $1
    record_id = $2

    DEF_OUT << "class #{to_n record} extends Protocol\n"
    DEF_OUT << "  @id = #{record_id}\n"
    DEF_OUT << "  constructor: (data = {}) -> data.id = #{to_n record}.id; super(data)\n"
    DEF_OUT << "  #{DEFINE[record.upcase]}\n\n"

    EXPORT_OUT << "  #{record.downcase}: #{to_n(record)}\n"
  end
end

parse_def "include/genesis_protocol.hrl"
parse_def "include/genesis_notify.hrl"

parse_wr "include/genesis_protocol.hrl"
parse_wr "include/genesis_notify.hrl"

#READ_OUT << ""
#WRITE_OUT << ""

puts "## auto generate command and notify protocol\n"
puts DEF_OUT
puts "\n"
puts EXPORT_OUT
