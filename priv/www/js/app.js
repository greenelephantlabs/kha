angular.module('Kha', ['ngResource']).
    factory('Project', function($resource){
        return $resource('/project/:id', {}, {
            query: {method:'GET',
                    params: {id: ''},
                    isArray:true}
        });
    }).
    factory('Build', function($resource){
        return $resource('/project/:projectId/build/:id', {}, {
            query: {method:'GET',
                    params: {projectId: '', id: ''},
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
        $scope.builds = Build.query({projectId: $scope.projectId});
    });

    $scope.getTotalBuilds = function () {
        return $scope.builds.length;
    };
}

BuildCtrl.$inject = ['$scope', 'Build'];
