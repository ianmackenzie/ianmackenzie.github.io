if ('window' in self) {
  if ('serviceWorker' in navigator) {
    self['Kintail'] = {}
    Kintail['Local'] = {}
    Kintail.Local['init'] = (function(thisFile) {
      function handleRequest(request) {
        if (request.method == 'GET' && request.path == 'local/file/read') {
          return Promise.resolve({status: 200, statusText: 'OK', body: "<file contents>"})
        } else {
          return Promise.resolve({status: 400, statusText: "Unrecognized request", body: ""})
        }
      }

      function registerServiceWorker() {
        return navigator.serviceWorker.register(thisFile).then(function(registration) {
          navigator.serviceWorker.addEventListener('message', function(event) {
            console.log('Received message from service worker', request)
            handleRequest(event.data).then(function(response) {
              event.ports[0].postMessage(response)
            })
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

  function requestFromClient(clientId, request) {
    return clients.get(clientId).then(function(client) {
      return new Promise(function(resolve, reject) {
        if (client !== undefined) {
          var messageChannel = new MessageChannel()

          messageChannel.port1.onmessage = function(event) {
            resolve(event.data)
          }
          
          console.log('Posting request to client', request)

          client.postMessage(request, [messageChannel.port2])
        } else {
          reject('Client not found')
        }
      })
    })
  }

  self.addEventListener('fetch', function(event) {
    var request = event.request
    var url = request.url
    if (url.startsWith('https://kintail/local/')) {
      return request.json().then(function(body) {      
        requestParameters = {
          method: request.method,
          path: url.slice(22),
          body: body
        }
        return requestFromClient(event.clientId, requestParameters).then(function(response) {
          return new Response(response.body, {status: response.status, statusText: response.statusText})
        })
      })
    } else {
      event.respondWith(fetch(event.request))
    }
  })
}
