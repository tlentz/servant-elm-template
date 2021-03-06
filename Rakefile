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
end

task :install => :build do
  sh("cd server && stack install --local-bin-path bin")
end

###############################################################################
# Docker
###############################################################################

task :docker_npm_install do
  sh( "docker exec -d servant-elm-template_npm_builder_1  bash -c " +
      "\"npm install && \"" +
      "\"npm install -g webpack@3.12.0"
    )
end

task :docker_stack_install do
  sh( "docker exec -d servant-elm-template_stack_builder_1  bash -c " +
      "\"cd server && stack install\""
    )
end

task :docker_build do
  sh("docker-compose build")
end

task :docker_build_client do
  sh("docker-compose up -d --no-deps --build npm_builder")
end

task :docker_build_server do
  sh("docker-compose up -d --no-deps --build stack_builder")
end  

task :docker_code_generator do
  sh( "docker exec -d servant-elm-template_stack_builder_1 bash -c " + 
      "\"cd server && stack exec code-generator\""
    )
end

task :docker_compose do
  imageID = `docker images -q servant-elm-template_npm_builder`
  if imageID == ""
    Rake::Task["docker_build"].execute
  else
    puts "Using container ID: #{imageID}. Any changes to dockerfiles will not be applied. " +
      "To apply changes use rake docker_build."
  end
  sh("docker-compose up -d")
end

task :docker_init => [:docker_compose, :rebuild_db_dev, :docker_stack_install, :docker_code_generator, :docker_npm_install]

task :docker_watch  do
  sh( "docker exec -d servant-elm-template_stack_builder_1 bash -c " +
    "\"cd server && stack exec app\""
    )
  sh( "docker exec servant-elm-template_npm_builder_1  bash -c " + 
      "\"cd /var/app/client && npm run watch &\""
    )
end

task :docker_clean => :clean do
  sh ( "docker exec servant-elm-template_npm_builder_1  bash -c " +
       "rm -rf node_modules && npm cache clear --force"
     )
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