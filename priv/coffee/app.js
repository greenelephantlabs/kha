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
