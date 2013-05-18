task :default =>[:buildapp]

desc "Uses buildapp to build a program suitable for the test suite"
task :buildapp do
  output = %x(buildapp --output jada --asdf-tree ~/quicklisp/dists/quicklisp/software/ \
--load-system jada --entry jada:run)
  puts output
end
