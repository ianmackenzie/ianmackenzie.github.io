if ('window' in self) {
  if ('serviceWorker' in navigator) {
    self['Kintail'] = {}
    Kintail['Local'] = {}
    Kintail.Local['init'] = (function(thisFile) {
      function registerServiceWorker() {
        return navigator.serviceWorker.register(thisFile).then(function(registration) {
          navigator.serviceWorker.addEventListener('message', function(event) {
            console.log('Received message from service worker', event.data)
          })
        }).catch(function(err) {
          console.log('ServiceWorker registration failed: ', err)
        })
      }

      return function() {
        return navigator.serviceWorker.getRegistration().then(function(registration) {
          if (registration === undefined) {
            return registerServiceWorker()
          } else {
            return registration.unregister().then(registerServiceWorker)
          }
        })
      }
    })(document.currentScript.src)
  } else {
    console.log('Service workers are not supported')
  }
} else if (self instanceof ServiceWorkerGlobalScope) {
  self.addEventListener('install', function(event) {
    event.waitUntil(self.skipWaiting())
  })

  self.addEventListener('activate', function(event) {
    event.waitUntil(self.clients.claim())
  })

  function requestFromClient(clientId, message) {
    return clients.get(clientId).then(function(client) {
      if (client !== undefined) {
        console.log('Posting message to client')
        
        var messageChannel = new MessageChannel()
        
        client.postMessage(message, [messageChannel.port2])
        return true
      } else {
        return false
      }

      // return new Promise(function(resolve, reject) {
      //   messageChannel.port1.onmessage = function(event) {
      //     if (event.data.error) {
      //       reject(event.data.error)
      //     } else {
      //       resolve(event.data)
      //     }
      //   }


      //   client.postMessage(message, [messageChannel.port2])
      // })
    })
  }

  self.addEventListener('fetch', function(event) {
    console.log('Intercepted fetch event for ' + event.request.url)
    if (event.request.url.startsWith('https://kintail/local/')) {
      requestFromClient(event.clientId, event.request.url.slice(22))
      event.respondWith(new Response("<file contents>", {status: 200, statusText: 'OK'}))
    } else {
      event.respondWith(fetch(event.request))
    }
  })
}
