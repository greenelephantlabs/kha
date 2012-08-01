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
    $scope.projects = Project.query();

    $scope.setCurrentProject = function(id) {
        $scope.projectId = id;
    }
    $scope.setCurrentProject(1);
}
ProjectCtrl.$inject = ['$scope', 'Project'];

function BuildCtrl($scope, Build) {
    $scope.predicate = 'id';
    $scope.builds = [];
    $scope.$watch('projectId', function(newValue, oldValue) {
        $scope.builds = Build.query({projectId: $scope.projectId, id: ''});
    });

    $scope.getTotalBuilds = function () {
        return $scope.builds.length;
    };

    $scope.rerun = function(build) {
        console.log('rerun', arguments);
        Build.rerun(build, $scope);
    }
    $scope.delete = function(build) {
        console.log('delete', arguments);
        build.$delete();
    }
}

BuildCtrl.$inject = ['$scope', 'Build'];
