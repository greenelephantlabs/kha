(function() {
  var BuildMVC;
  var __hasProp = Object.prototype.hasOwnProperty, __extends = function(child, parent) {
    for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; }
    function ctor() { this.constructor = child; }
    ctor.prototype = parent.prototype;
    child.prototype = new ctor;
    child.__super__ = parent.prototype;
    return child;
  };
  BuildMVC = (function() {
    __extends(BuildMVC, Batman.App);
    function BuildMVC() {
      BuildMVC.__super__.constructor.apply(this, arguments);
    }
    BuildMVC.root('builds#index');
    return BuildMVC;
  })();
  BuildMVC.JSONRestStorage = (function() {
    __extends(JSONRestStorage, Batman.RestStorage);
    function JSONRestStorage() {
      JSONRestStorage.__super__.constructor.apply(this, arguments);
    }
    JSONRestStorage.prototype.serializeAsForm = false;
    return JSONRestStorage;
  })();
  window.BuildMVC = BuildMVC;
  BuildMVC.run();
}).call(this);
(function() {
  var __hasProp = Object.prototype.hasOwnProperty, __extends = function(child, parent) {
    for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; }
    function ctor() { this.constructor = child; }
    ctor.prototype = parent.prototype;
    child.prototype = new ctor;
    child.__super__ = parent.prototype;
    return child;
  };
  BuildMVC.Build = (function() {
    __extends(Build, Batman.Model);
    function Build() {
      Build.__super__.constructor.apply(this, arguments);
    }
    Build.global = true;
    Build.persist(BuildMVC.JSONRestStorage);
    Build.encode('body', 'isDone');
    Build.prototype.body = '';
    Build.prototype.isDone = false;
    return Build;
  })();
}).call(this);
(function() {
  var __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; }, __hasProp = Object.prototype.hasOwnProperty, __extends = function(child, parent) {
    for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; }
    function ctor() { this.constructor = child; }
    ctor.prototype = parent.prototype;
    child.prototype = new ctor;
    child.__super__ = parent.prototype;
    return child;
  };
  BuildMVC.BuildsController = (function() {
    __extends(BuildsController, Batman.Controller);
    function BuildsController() {
      this.create = __bind(this.create, this);
      BuildsController.__super__.constructor.apply(this, arguments);
    }
    BuildsController.prototype.emptyBuild = new BuildMVC.Build();
    BuildsController.prototype.index = function() {
      this.set('emptyBuild', new BuildMVC.Build());
      return this.render(false);
    };
    BuildsController.prototype.edit = function(node, event) {
      return $(node).addClass('editing');
    };
    BuildsController.prototype.update = function(node, event) {
      var newBuild;
      newBuild = new BuildMVC.Build({
        id: $(node).attr('id'),
        body: $(node).val()
      });
      newBuild.save();
      return $(node).parent().removeClass('editing');
    };
    BuildsController.prototype.create = function() {
      return this.emptyBuild.save(__bind(function(error, record) {
        return this.set('emptyBuild', new BuildMVC);
      }, this));
    };
    return BuildsController;
  })();
}).call(this);
