angular.module('Kha').controller({
    LoginController: function ($scope, $http, authService) {
        $scope.auth = authService;
        $scope.urgent = false;
        $scope.login = function() {
            $http.post('user/login', {email: $scope.email, password: $scope.password}).success(function(data) {
                if (data.result === true) {
                    authService.loginConfirmed(data.session);
                }
            }).error(function() {
                authService.logout();
                alert('Login fail!');
            });
        };
        $scope.logout = function() {
            $http.post('/user/logout', {}).success(function() {
                authService.logout();
            });
        };
        $http.get('/user/session').success(function(data) {
            if (data.result === true) {
                authService.setSession(data.session);
            };
        });
        $scope.$on('event:auth-loginRequired', function() {
            if (authService.getSession()) {
                alert("It seems that you don't have permissions to perform this operation!");
                // this does not drop the operation! and it is still queued in the authService
            } else {
                $scope.urgent = true;
            }
        });
        $scope.$on('event:auth-loginConfirmed', function() {
            $scope.urgent = false;
        });
    }
});
