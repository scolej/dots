old =
  `git branch --merged origin/master`.split("\n")
    .reject { |line| line.include?("*") || line.include?("+") }
    .map { |line| line.strip }
if old.empty?
  puts "no stale branches"
else
  puts "found stale branches:"
  puts old
  puts "deleting them..."
  old.each { |o| system("git", "branch", "-d", o) }
end

puts "running some git maintenance"
system("git gc")
