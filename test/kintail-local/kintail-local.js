if ('window' in self) {
  if ('serviceWorker' in navigator) {
    self['Kintail'] = {}
    Kintail['Local'] = {}
    Kintail.Local['init'] = (function(thisFile) {
      function registerServiceWorker() {
        console.log('Registering service worker')
        return navigator.serviceWorker.register(thisFile).then(function(registration) {
          console.log('ServiceWorker registration successful with scope: ', registration.scope);
        }).catch(function(err) {
          console.log('ServiceWorker registration failed: ', err);
        });
      }

      return function() {
        return navigator.serviceWorker.getRegistration().then(function(registration) {
          if (registration === undefined) {
            console.log('No existing service worker')
            return registerServiceWorker()
          } else {
            console.log('Unregistering existing service worker')
            return registration.unregister().then(registerServiceWorker);
          }
        });
      }
    })(document.currentScript.src)
  } else {
    console.log('Service workers are not supported')
  }
} else if (self instanceof ServiceWorkerGlobalScope) {
  self.addEventListener('fetch', function(event) {
    console.log('Intercepted fetch event for ' + event.request.url)
  });
}
