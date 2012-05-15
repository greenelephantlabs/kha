class BuildMVC.BuildsController extends Batman.Controller
  emptyBuild: new BuildMVC.Build()

  index: ->
    @set 'emptyBuild', new BuildMVC.Build()
    @render false

   edit: (node, event) ->
     $(node).addClass('editing')

   update: (node, event) ->
     newBuild = new BuildMVC.Build({id: $(node).attr('id'), body: $(node).val()})
     newBuild.save()
     $(node).parent().removeClass('editing')

  create: =>
    @emptyBuild.save (error, record)  =>
       @set 'emptyBuild', new BuildMVC
