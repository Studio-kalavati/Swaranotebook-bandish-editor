module.exports = {
	globDirectory: 'resources/public/',
        globPatterns: [
               '**/*.{png,css,eot,svg,ttf,woff,woff2}',
       ],

	swDest: 'resources/public/sw.js',
	swSrc: 'sw-src.js',
	maximumFileSizeToCacheInBytes: 15000000
};
