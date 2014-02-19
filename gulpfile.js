var gulp = require('gulp'),
    sass = require('gulp-ruby-sass'),
    autoprefixer = require('gulp-autoprefixer'),
    jshint = require('gulp-jshint'),
    rename = require('gulp-rename'),
    clean = require('gulp-clean'),
    concat = require('gulp-concat'),
    notify = require('gulp-notify'),
    handlebars = require('gulp-ember-handlebars'),
    declare = require('gulp-declare');

gulp.task('styles', function() {
  return gulp.src('resources/styles/main.scss')
    .pipe(sass({ style: 'expanded' }))
    .pipe(gulp.dest('resources/dist/css'))
    .pipe(notify({ message: 'Styles task complete' }));
});

gulp.task('scripts', function() {
  return gulp.src('resources/js/**/*.js')
  // .pipe(jshint('.jshintrc'))
    .pipe(jshint.reporter('default'))
    .pipe(concat('main.js'))
    .pipe(gulp.dest('resources/dist/js'))
    .pipe(notify({ message: 'Scripts task complete' }));
});

gulp.task('clean', function() {
  return gulp.src(['resources/dist/css', 'resources/dist/js'], {read: false})
    .pipe(clean());
});

gulp.task('templates', function(){
  gulp.src(['resources/templates/**/*.hbs'])
    .pipe(handlebars({
      outputType: 'browser'
    })).pipe(concat('templates.js'))
    .pipe(gulp.dest('resources/dist/js/'));
});

gulp.task('watch', function() {

  // // Watch .scss files
  // gulp.watch('resources/styles/**/*.scss', ['styles']);

  // Watch .js files
  gulp.watch('resources/js/**/*.js', ['scripts']);

  // Watch image files
  gulp.watch('resources/templates/**/*hbs', ['templates']);

});
