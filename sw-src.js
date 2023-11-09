importScripts(
  'https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js'
);

workbox.precaching.precacheAndRoute(self.__WB_MANIFEST);

workbox.routing.registerRoute(/\.(?:png|gif|jpg|jpeg|woff|woff2|eot|ttf|svg)$/, new workbox.strategies.CacheFirst());

workbox.routing.registerRoute(
  ({url}) => url.pathname.startsWith('/view/'),
  new workbox.strategies.NetworkFirst()
);

workbox.routing.registerRoute(
  /\.(?:js|html|css)$/,
  new workbox.strategies.StaleWhileRevalidate()
);
