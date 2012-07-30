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
