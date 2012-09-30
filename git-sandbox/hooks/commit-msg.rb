#!/usr/bin/env ruby

msg_file = ARGV[0]
msg = File.read(msg_file)

$regex = /\[ref: (\d+)\]/

if ! $regex.match(msg)
    puts "[ Policy ] Msg format is incorrect."
    exit 1
end
