angular.module('Kha', ['http-auth-interceptor', 'ngResource']).
    factory('Project', function($resource){
        var r = $resource('/project/:id', {id: '@id'}, {
            query: {method:'GET',
                    params: {id: ''},
                    isArray:true}
        });
        r.prototype.build = [];
        r.prototype.notifications = [];
        return r;
    }).
    factory('Build', function($resource){
        var b = $resource('/project/:projectId/build/:id?limit=:limit&last=:last',
                          {projectId: '@project',
                           id: '@id',
                           limit: '@limit',
                           last: '@last'}, {
            query: {method:'GET',
                    params: {limit: 0},
                    isArray:true},
            do_rerun: {method: 'POST',
                       params: {id: ''}}
        });
        b.rerun = function(build, $scope) {
            b.do_rerun({project: build.project,
                        copy: build.id},
                       function(newBuild) {
                           $scope.builds[newBuild.id] = newBuild;
                       });
        };
        return b;
    });

function ProjectCtrl($scope, $location, Project) {
    $scope.predicate = 'id';
    $scope.$location = $location;
    $scope.$watch('$location.path()', function(newValue, oldValue) {
        if (!newValue)
            return;
        var x = newValue.split('/');
        if (x[1] == 'project') {
            var pid = parseInt(x[2]);
            if (pid !== parseInt(null)) {
                var p = _.groupBy($scope.projects, 'id')[pid];
                if (p && p[0]) {
                    $scope.currentProject = p[0];
                }
            }
        }
    });

    $scope.addProject = function() {
        $scope.currentProject = new Project({});
        $scope.selectTab('details');
        $scope.$broadcast('add_project', 1, 2, 3);
    };

    $scope.$on('new_project', function(e, project) {
        $scope.projects.push(project);
    });

    $scope.$watch('currentProject', function(newValue, oldValue) {
        if (newValue === null)
            return;
        $scope.currentProject = newValue;
        $scope.currentBuild = null;
        if ($scope.tab == 'build') {
            $scope.selectTab('builds');
        }
    });

    $scope.projects = Project.query(function(projects) {
        if (projects)
            $scope.currentProject = projects[0];
    });
    $scope.currentProject = null;
    $scope.currentBuild = null;
    $scope.getProjectClass = function(project) {
        return project === $scope.currentProject ? 'active' : '';
    }

    $scope.tab = 'builds';
    $scope.getTabClass = function(type) {
        return $scope.tab === type ? 'active' : '';
    }
    $scope.selectTab = function(tab) {
        $scope.tab = tab;
    }

    $scope.showBuildDetails = function(build) {
        $scope.currentBuild = build;
        $scope.selectTab('build')
        $location.path('/project/'+$scope.currentProject.id+ '/build/'+build.id);
    }
}
ProjectCtrl.$inject = ['$scope', '$location', 'Project'];

function DetailsCtrl($scope) {
    $scope.editing = false;
    var backup = null;
    function edit() {
        $scope.editing = true;
        backup = _.extend({}, $scope.currentProject);
        backup.build = _.toArray($scope.currentProject.build);
        backup.notifications = _.toArray($scope.currentProject.notifications);
    }
    $scope.$on('add_project', function() {
        edit();
    });
    $scope.edit = edit;
    $scope.cancel = function() {
        $scope.editing = false;
        _.extend($scope.currentProject, backup);
    }
    $scope.save = function() {
        $scope.editing = false;
        $scope.currentProject.build = _.filter($scope.currentProject.build, function(x) { return !_.isEmpty(x) });
        var old = 'id' in $scope.currentProject;
        $scope.currentProject.$save(function() {
            if (!old) {
                $scope.$emit('new_project', $scope.currentProject);
            }
        });
    }
    $scope.getEditingClass = function() {
        return $scope.editing ? 'editing' : '';
    }
}

function BuildCtrl($scope, $window, $timeout, Build) {
    $scope.predicate = 'id';
    $scope.builds = {};
    $scope.branch = 'master';
    $scope.values = _.values;

    function updateBuilds(builds, append) {
        append = append || false;
        var blds = $scope.builds;
        var bk = _.groupBy(builds, 'id');
        if (!append) {
            _.each(blds, function(ob) {
                if (!(ob.id in bk)) {
                    delete blds[ob.id];
                }
            });
        }
        _.each(builds, function(b) {
            if (b.id in blds) {
                _.extend(blds[b.id], b);
            } else {
                blds[b.id] = b;
            }
        });
    }

    $scope.$watch('currentProject.id', function(newValue, oldValue) {
        if (!newValue) return;
        Build.query({projectId: newValue, id: '', limit: BuildCtrl.page_size}, function(builds) {
            updateBuilds(builds);
        });
    });

    $scope.run = function(branch) {
        $scope.currentBuild = new Build({
            project: $scope.currentProject.id,
            title: 'manual build at '+branch,
            branch: branch,
            revision: '',
            author: 'web user',
            tags: ['manual']
        });
        $scope.currentBuild.$save(function(build) {
            $scope.builds[build.id] = build;
        });
    }

    $scope.rerun = function(build, $event) {
        Build.rerun(build, $scope);
        $event.stopPropagation();
    }
    $scope.delete = function(build, $event) {
        build.$delete(function() {
            delete $scope.builds[build.id];
        });
        $event.stopPropagation();
    }

    $scope.loadMore = function() {
        var last = _.min(_.map(_.keys($scope.builds), function(x) {
            return parseInt(x);
        }));
        Build.query({projectId: $scope.currentProject.id, id: '', limit: BuildCtrl.page_size, last: last}, function(builds) {
            updateBuilds(builds, true);
        });
    }

    $scope.$watch('builds', function(builds, oldValue) {
        if (!builds) return;
        if (!$scope.currentBuild) return;

        var b = _.groupBy(builds, 'id')[$scope.currentBuild.id];
        if (b && b[0]) {
            _.extend($scope.currentBuild, b[0]);
        }
    });

    $timeout(function fetch(){
        if (!$scope.currentProject.id) return;
        if (!$scope.builds) return;
        Build.query({projectId: $scope.currentProject.id, id: '', limit: _.size($scope.builds)}, function(builds) {
            updateBuilds(builds, true);
            $timeout(fetch, 5000);
        });
    }, 5000);
}

BuildCtrl.page_size = 10;

BuildCtrl.$inject = ['$scope', '$window', '$timeout', 'Build'];
