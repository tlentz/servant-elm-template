require 'standalone_migrations'
StandaloneMigrations::Tasks.load_tasks

task :default => [:build_all, :serve]

task :build_all => [:build_backend, :build_frontend]

task :build_backend do
    sh("stack build")
end

task :build_frontend => [:elm, :fuse]

task :elm => [:elm_api_code_generator, :elm_app_js]

task :elm_api_code_generator do
    mkdir_p "frontend/elm/Shared/"
    sh("stack exec code-generator")
end

task :elm_app_js do
    mkdir_p "frontend/dist/"
    sh("cd frontend && elm-make elm/Main.elm --output static/app.js")
end

task :fuse do
    sh("cd frontend && node fuse copy && node fuse")
end


task :serve do
    sh("stack exec app")
end