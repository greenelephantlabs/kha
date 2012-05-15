class BuildMVC.Build extends Batman.Model
  @global: yes

  @persist BuildMVC.JSONRestStorage #Batman.RestStorage
  @encode 'body', 'isDone'

  body: ''
  isDone: false
