/**
 * Created by jeff on 23/03/2015.
 */
var gulp = require('gulp');
var sass = require('gulp-sass');
var uglify = require('gulp-uglify');
var concat = require('gulp-concat');
var postcss = require('gulp-postcss');
var sourcemaps = require('gulp-sourcemaps');
var autoprefixer = require('autoprefixer-core');

var paths = {
	styles:        ['./assets/styles/app.scss'],
	styleIncludes: [
		'./assets/styles',
		'./bower_components/foundation/scss'
	],
	scripts:       [
		'./bower_components/jquery/dist/jquery.js',
		'./bower_components/foundation/js/foundation/foundation.js',
		'./bower_components/foundation/js/foundation/foundation.interchange.js',
		'./bower_components/fastclick/lib/fastclick.js',
		'./bower_components/bacon/dist/Bacon.js',
		'./assets/scripts/app.js'
	]
};

gulp.task('styles', function () {
	return gulp.src(paths.styles)
		.pipe(sourcemaps.init())
		.pipe(sass({includePaths: paths.styleIncludes}))
		.pipe(postcss([autoprefixer({browsers: ['last 2 version']})]))
		.pipe(concat('app.css'))
		.pipe(sourcemaps.write('.'))
		.pipe(gulp.dest('../public/stylesheets'))
});

gulp.task('scripts', function () {
	gulp.src(paths.scripts)
		.pipe(sourcemaps.init())
		.pipe(uglify())
		.pipe(concat('app.js'))
		.pipe(sourcemaps.write('.'))
		.pipe(gulp.dest('../public/javascripts'));

	return gulp
		.src(['./bower_components/modernizr/modernizr.js'])
		.pipe(gulp.dest('../public/javascripts'))
});

gulp.task('watch', ['styles', 'scripts'], function () {
	gulp.watch(paths.styles.concat(paths.styleIncludes), ['styles']);
	gulp.watch(paths.scripts, ['scripts']);
});

gulp.task('default', ['watch']);