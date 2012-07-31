angular.module('Kha', ['ngResource']).
    factory('Project', function($resource){
        return $resource('/project/:id', {}, {
            query: {method:'GET',
                    params: {id: ''},
                    isArray:true}
        });
    }).
    factory('Build', function($resource){
        return $resource('/project/:projectId/build/:id', {projectId: '@project', id: '@id'}, {
            query: {method:'GET',
                    params: {},
                    isArray:true}
        });
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
    $scope.builds = [];
    $scope.$watch('projectId', function(newValue, oldValue) {
        $scope.builds = Build.query({projectId: $scope.projectId, id: ''});
    });

    $scope.getTotalBuilds = function () {
        return $scope.builds.length;
    };

    $scope.rerun = function(build) {
        console.log('rerun', arguments);
        build.$save();
    }
    $scope.delete = function(build) {
        console.log('delete', arguments);
        build.$delete();
    }
}

BuildCtrl.$inject = ['$scope', 'Build'];
