#!/usr/bin/env ruby

puts 'fetching all remotes'
`git remote update -p`

old =
  `git branch --merged origin/master`.split("\n")
    .reject { |line| line.include?("*") || line.include?("+") }
    .map { |line| line.strip }
if old.empty?
  puts "no stale branches"
else
  puts "found stale branches, deleting them..."
  old.each { |o| system("git", "branch", "-d", o) }
end

puts "running some git maintenance"
system("git prune")
system("git gc")
