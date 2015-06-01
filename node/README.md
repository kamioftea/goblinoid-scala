Scala Goblinoid Node Dependencies
=================================

This is internal to the goblinoid-scala project and is used to compile
scss/js to minified/uglified concatenated files.
 
Ideally this will eventually be migrated to use the sbt/webjars solution
that comes with Play, but until I or someone else writes webjar wrappers 
for the gulp tools I'm used to this will have to do

On initial download run 

    npm install
    bower install
    
And if you don't have it already:

	npm install -g gulp
	
Then to build and watch the js/scss

	gulp