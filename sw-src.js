importScripts(
  'https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js'
);

self.__WB_DISABLE_DEV_LOGS = true;
workbox.precaching.precacheAndRoute(self.__WB_MANIFEST);

workbox.routing.registerRoute(/\.(?:png|gif|jpg|jpeg|woff|woff2|eot|ttf|svg|mp3)$/, new workbox.strategies.CacheFirst());

workbox.routing.registerRoute(
  ({url}) => url.pathname.startsWith('/view/'),
  new workbox.strategies.NetworkFirst()
);

workbox.routing.registerRoute(
  ({url}) => url.pathname.startsWith('/js/compiled/'),
  new workbox.strategies.NetworkFirst()
);

workbox.routing.registerRoute(
  /\.(?:html|css)$/,
  new workbox.strategies.StaleWhileRevalidate()
);
