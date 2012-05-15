class BuildMVC extends Batman.App
  @root 'builds#index'

  # @model 'build'
  # @controller 'builds'

class BuildMVC.JSONRestStorage extends Batman.RestStorage
  serializeAsForm: false

window.BuildMVC = BuildMVC

# Start the app
BuildMVC.run()