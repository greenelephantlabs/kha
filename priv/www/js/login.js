angular.module('Kha').controller({
  LoginController: function ($scope, $http, authService) {
    $scope.submit = function() {
      $http.post('user/login', {email: $scope.email, password: $scope.password}).success(function() {
        authService.loginConfirmed();
      }).error(function() {
        alert('Login fail!');
      });
    }
  }
});
