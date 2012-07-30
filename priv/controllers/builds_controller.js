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
