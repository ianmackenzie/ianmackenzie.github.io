if ('window' in self) {
  if ('serviceWorker' in navigator) {
    self['Kintail'] = {}
    Kintail['Local'] = {}
    Kintail.Local['init'] = (function(thisFile) {
      function registerServiceWorker() {
        console.log('Registering service worker')

        return navigator.serviceWorker.register(thisFile).then(function(registration) {
          console.log('ServiceWorker registration successful with scope: ', registration.scope)
        
          navigator.serviceWorker.addEventListener('message', function(event) {
            console.log('Received message from service worker', event.data)
          });
        }).catch(function(err) {
          console.log('ServiceWorker registration failed: ', err)
        })
      }

      return function() {
        return navigator.serviceWorker.getRegistration().then(function(registration) {
          if (registration === undefined) {
            console.log('No existing service worker')

            return registerServiceWorker()
          } else {
            console.log('Unregistering existing service worker')

            return registration.unregister().then(registerServiceWorker)
          }
        })
      }
    })(document.currentScript.src)
  } else {
    console.log('Service workers are not supported')
  }
} else if (self instanceof ServiceWorkerGlobalScope) {
  function requestFromClient(clientId, message) {
    console.log('Looking up client from ID', clientId)

    return clients.get(clientId).then(function(client) {
      console.log('Found client')

      return new Promise(function(resolve, reject) {
        var messageChannel = new MessageChannel();
        
        messageChannel.port1.onmessage = function(event) {
          if (event.data.error) {
            reject(event.data.error);
          } else {
            resolve(event.data);
          }
        };

        console.log('Posting message to client')
        
        client.postMessage(message, [messageChannel.port2]);
      });
    })
  }

  self.addEventListener('fetch', function(event) {
    console.log('Intercepted fetch event for ' + event.request.url)

    requestFromClient(event.clientId, event.request.url)
  })
}
