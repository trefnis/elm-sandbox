;(function (app) {
  app.ports.selectText.subscribe(function(event) {
    var element = event.target;

    if ('select' in element) {
      element.select();
    }
  });

}(window.app));