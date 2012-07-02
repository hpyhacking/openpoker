#!/usr/bin/ruby
#
DEFINE = Hash.new
READ_OUT = "%% read binary to protocol record\n"
WRITE_OUT = "%% write protocol record to binary\n"

def parse_def file
  File.open(file).each_line do |line| 
    next unless line =~ /record\((cmd_[a-z_]*|notify_[a-z_]*),\s*\{(.*)\}/

    record = $1
    internal = false

    record_def = ""
    record_def << "record(#{record}, {"
    record_def << $2.split(',').map do |x| 
      f = x.strip
      f = (f == "game" and record =~ /^notify/) ? "game_id" : f
      f = (f == "player" and record =~ /^notify/) ? "player_id" : f

      internal = true if (internal == false and f == "'|'")

      internal ? "internal()" : "#{f}()"
    end.join(", ")
    record_def << "})"
    DEFINE[record.upcase] = record_def
  end
end

def parse_wr file
  File.open(file).each_line do |line| 
    next unless line =~ /^\-define\((CMD_[A-Z_]*|NOTIFY_[A-Z_]*)/

    READ_OUT << "read(<<?#{$1}, Bin/binary>>) ->\n"
    READ_OUT << "  unpickle(#{DEFINE[$1]}, Bin);\n\n"

    WRITE_OUT << "write(R) when is_record(R, #{$1.downcase}) ->\n"
    WRITE_OUT << "  [?#{$1} | pickle(#{DEFINE[$1]}, R)];\n\n"
  end
end

parse_def "include/genesis_protocol.hrl"
parse_def "include/genesis_notify.hrl"

parse_wr "include/genesis_protocol.hrl"
parse_wr "include/genesis_notify.hrl"

READ_OUT << "read(_ErrorBin) -> ok.\n"
WRITE_OUT << "write(_ErrorRecord) -> ok.\n"

puts "%% auto generate command and notify protocol\n"
puts READ_OUT
puts "\n"
puts WRITE_OUT
