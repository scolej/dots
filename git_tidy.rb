old = `git branch --merged origin/master`.split("\n")
puts "found stale branches #{old}"
old.each do |o|
    system("git branch -d #{o}")
end
    
system('git gc')
