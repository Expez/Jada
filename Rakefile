task :default =>[:buildapp]

desc "Uses buildapp to build a program suitable for the test suite"
task :buildapp do
  output = %x(buildapp --output jada --asdf-tree ~/quicklisp/dists/quicklisp/software/ \
--load-system jada --entry jada:run)
  success = $?.exitstatus == 0
  if success
    puts "Successfully created executable."
  else
    puts "Failed to create execute!"
    puts output
  end
end
