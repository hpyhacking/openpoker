#!/usr/bin/ruby
#
DEFINE = Hash.new
DEF_OUT =  "class TestProtocol extends Protocol\n"
DEF_OUT << "  @id = 255\n"
DEF_OUT << "  constructor: (data = {}) -> data.id = TestProtocol.id; super(data)\n"
DEF_OUT << "  Protocol.reg @id, {int: integer, str: string, byte: byte, base: base64, coin: coin}"

EXPORT_OUT =  "window.protocols =\n"
EXPORT_OUT << "  test: TestProtocol\n"

TYPES = []

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
      TYPES << f if not internal
      internal ? nil : "#{f}: type"
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

    DEF_OUT << "class #{record} extends Protocol\n"
    DEF_OUT << "  @id = #{record_id}\n"
    DEF_OUT << "  constructor: (data = {}) -> data.id = #{record}.id; super(data)\n"
    DEF_OUT << "  #{DEFINE[record.upcase]}\n\n"

    EXPORT_OUT << "  #{record.downcase}: #{record}\n"
  end
end

parse_def "include/genesis_protocol.hrl"
parse_def "include/genesis_notify.hrl"

parse_wr "include/genesis_protocol.hrl"
parse_wr "include/genesis_notify.hrl"

#READ_OUT << ""
#WRITE_OUT << ""

puts "%% auto generate command and notify protocol\n"
puts DEF_OUT
puts "\n"
puts EXPORT_OUT

puts TYPES.uniq
