module.exports = {
	globDirectory: 'resources/public/',
	globPatterns: [
		'**/*.{png,html,css,eot,svg,ttf,woff,woff2}',
		'js/compiled/app.js',
		'js/compiled/manifest.edn'
	],
	swDest: 'resources/public/sw.js',
	maximumFileSizeToCacheInBytes: 15000000,
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	]
};
