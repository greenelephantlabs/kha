angular.module('Kha', ['ngResource']).
    factory('Project', function($resource){
        return $resource('/project/:id', {}, {
            query: {method:'GET',
                    params: {id: ''},
                    isArray:true}
        });
    }).
    factory('Build', function($resource){
        var b = $resource('/project/:projectId/build/:id', {projectId: '@project', id: '@id'}, {
            query: {method:'GET',
                    params: {},
                    isArray:true},
            do_rerun: {method: 'POST',
                       params: {id: ''}}
        });
        b.rerun = function(build, $scope) {
            b.do_rerun({project: build.project,
                        copy: build.id},
                       function(newBuild) {
                           $scope.builds.push(newBuild);
                       });
        };
        return b;
    });

function ProjectCtrl($scope, Project) {
    $scope.projects = Project.query(function(projects) {
        if (projects)
            $scope.setCurrentProject(projects[0]);
    });
    $scope.currentProject = null;
    $scope.currentBuild = null;
    $scope.setCurrentProject = function(project) {
        $scope.currentProject = project;
        $scope.currentBuild = null;
        $scope.selectTab('builds');
    }
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
        $scope.selectTab('build-details')
    }
    // $scope.printBuildDetails = function(b) {
    //     if (!b)
    //     return b.output;
    // }
}
ProjectCtrl.$inject = ['$scope', 'Project'];

function BuildCtrl($scope, $window, $timeout, Build) {
    $scope.predicate = 'id';
    $scope.builds = [];

    $scope.$watch('currentProject', function(newValue, oldValue) {
        if (newValue === null)
            return;
        $scope.builds = Build.query({projectId: $scope.currentProject.id, id: ''});
    });

    $scope.getTotalBuilds = function () {
        return $scope.builds.length;
    };

    $scope.rerun = function(build, $event) {
        Build.rerun(build, $scope);
        $event.stopPropagation();
    }
    $scope.delete = function(build, $event) {
        build.$delete(function() {
            $scope.builds = _.without($scope.builds, build);
        });
        $event.stopPropagation();
    }

    $timeout(function updateBuilds(){
        Build.query({projectId: $scope.currentProject.id, id: ''}, function(builds) {
            $scope.builds = builds;
            $timeout(updateBuilds, 5000);
        });
    }, 5000);
}

BuildCtrl.$inject = ['$scope', '$window', '$timeout', 'Build'];
