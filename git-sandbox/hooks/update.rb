#!/usr/bin/env ruby

$refname = ARGV[0]
$oldrev = ARGV[1]
$newrev = ARGV[2]
$user = "haisgwu"

$regex = /\[ref: (\d)\]/

def check_msg_format
  missed_revs = `git rev-list #{ $oldrev}..#{ $newrev}`.split("\n")
  missed_revs.each do |rev|
    msg = `git cat-file commit #{ rev} | sed '1,/^$/d'`
    puts "the message of commit: #{rev} is: #{msg}"
    if !$regex.match(msg)
      puts "[ Push Failed ] Your msg is not formatted correctly "
      exit 1
    end
  end
end
check_msg_format
