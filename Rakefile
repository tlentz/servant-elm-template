require 'standalone_migrations'
StandaloneMigrations::Tasks.load_tasks

task :watch => [:build_backend, :elm_api_code_generator, :serve_webpack_hot_reload]

task :build => [:build_backend, :elm_api_code_generator, :build_frontend]

multitask :serve_webpack_hot_reload => [:serve, :webpack_hot_reload]

task :build_backend do
  sh("cd server && stack build")
end

task :serve do
  sh("cd server && stack exec app")
end

task :webpack_hot_reload do
  sh("cd client && npm run watch")
end

task :build_frontend do
  sh("cd client && npm run build")
end

task :elm_api_code_generator do
  mkdir_p "client/elm/Generated/"
  sh("cd server && stack exec code-generator")
end

task :npm do
  sh("cd client && npm install")
end

task :elm do
  sh("cd client && elm package install --yes")
end

task :clean do
  rm_rf "server/bin/"
  rm_rf "client/dist/"
  rm Dir.glob("*.zip")
  sh("cd server && stack clean")
end

task :install => :build do
  sh("cd server && stack install --local-bin-path bin")
end

task :docker do
  sh("docker build -t servant-elm-example .") 
end


###############################################################################
# DB MIGRATIONS
###############################################################################
task :rebuild_db_dev do
  sh("rake db:drop RAILS_ENV=development")
  sh("rake db:create RAILS_ENV=development")
end

task :db_create_migration do
  prefix = Time.now.utc.strftime("%Y%m%d%H%M%S")
  label = ENV['name']
  filename = "#{prefix}_#{label}.sql"
  if (filename =~ /\A([0-9]+)_([_a-z0-9]*).sql\z/)
    outfile = "./db/migrate/#{filename}"
    FileUtils.touch(outfile)
    puts "created migration: #{outfile}"
  else
    puts "Invalid 'name' parameter: '#{label}'. Only lowercase letters, numbers, and underscores allowed."
  end
end
###############################################################################
###############################################################################