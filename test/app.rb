require 'sinatra'
require 'sinatra/contrib'
require "sinatra/reloader" if development?

post '/play' do
  request.body.rewind
  p request.body.read
  p request.content_length 
end

put '/photo' do
end

post '/stop' do
  p 'a'
end

