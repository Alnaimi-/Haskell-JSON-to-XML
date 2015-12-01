import Network.HTTP

main = do
  response <- simpleHTTP $ getRequest "http://www.json-generator.com/api/json/get/cspVKDNjJu?indent=4"
  let body = fmap rspBody response
  print response
  print body