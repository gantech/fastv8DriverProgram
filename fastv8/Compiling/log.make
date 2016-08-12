g++  -O2 -g  -fPIC -fpermissive -c ../Source/FAST_cInterface.c -I../Source/dependencies/OpenFOAM/ -I../Source/dependencies/yaml-cpp/include/ -o Obj_lin64/FAST_cInterface.o 
In file included from /nopt/nrel/apps/compiler/gcc/4.8.1/include/c++/4.8.1/array:35:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/convert.h:10,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:18,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
/nopt/nrel/apps/compiler/gcc/4.8.1/include/c++/4.8.1/bits/c++0x_warning.h:32:2: error: #error This file requires compiler and library support for the ISO C++ 2011 standard. This support is currently experimental, and must be enabled with the -std=c++11 or -std=gnu++11 compiler options.
 #error This file requires compiler and library support for the \
  ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:10:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/parser.h:41:28: warning: explicit conversion operators only available with -std=c++11 or -std=gnu++11 [enabled by default]
   explicit operator bool() const;
                            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/parser.h:81:3: error: ‘unique_ptr’ in namespace ‘std’ does not name a type
   std::unique_ptr<Scanner> m_pScanner;
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/parser.h:82:3: error: ‘unique_ptr’ in namespace ‘std’ does not name a type
   std::unique_ptr<Directives> m_pDirectives;
   ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:11:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/emitter.h:125:3: error: ‘unique_ptr’ in namespace ‘std’ does not name a type
   std::unique_ptr<EmitterState> m_pState;
   ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:14:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/exceptions.h:115:22: error: expected ‘;’ at end of member declaration
   virtual ~Exception() noexcept {}
                      ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/exceptions.h:115:24: error: ‘noexcept’ does not name a type
   virtual ~Exception() noexcept {}
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/exceptions.h:117:33: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   Exception(const Exception&) = default;
                                 ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/exceptions.h:115:11: error: looser throw specifier for ‘virtual YAML::Exception::~Exception()’
   virtual ~Exception() noexcept {}
           ^
In file included from ../Source/FAST_cInterface.h:10:0,
                 from ../Source/FAST_cInterface.c:1:
/nopt/nrel/apps/compiler/gcc/4.8.1/include/c++/4.8.1/stdexcept:121:13: error:   overriding ‘virtual std::runtime_error::~runtime_error() throw ()’
     virtual ~runtime_error() _GLIBCXX_USE_NOEXCEPT;
             ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:14:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/exceptions.h:168:29: error: expected ‘;’ at end of member declaration
   virtual ~TypedKeyNotFound() noexcept {}
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/exceptions.h:168:31: error: ‘noexcept’ does not name a type
   virtual ~TypedKeyNotFound() noexcept {}
                               ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/node.h:17:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:16,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/ptr.h:21:9: error: ‘shared_ptr’ in namespace ‘std’ does not name a type
 typedef std::shared_ptr<node> shared_node;
         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/ptr.h:22:9: error: ‘shared_ptr’ in namespace ‘std’ does not name a type
 typedef std::shared_ptr<node_ref> shared_node_ref;
         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/ptr.h:23:9: error: ‘shared_ptr’ in namespace ‘std’ does not name a type
 typedef std::shared_ptr<node_data> shared_node_data;
         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/ptr.h:24:9: error: ‘shared_ptr’ in namespace ‘std’ does not name a type
 typedef std::shared_ptr<memory_holder> shared_memory_holder;
         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/ptr.h:25:9: error: ‘shared_ptr’ in namespace ‘std’ does not name a type
 typedef std::shared_ptr<memory> shared_memory;
         ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:16:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/node.h:119:45: error: ‘YAML::detail::shared_memory_holder’ has not been declared
   explicit Node(detail::node& node, detail::shared_memory_holder pMemory);
                                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/node.h:133:11: error: ‘shared_memory_holder’ in namespace ‘YAML::detail’ does not name a type
   mutable detail::shared_memory_holder m_pMemory;
           ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:12:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/iterator.h:13,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:11,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h:40:43: error: ‘>>’ should be ‘> >’ within a nested template argument list
 typedef std::vector<std::pair<node*, node*>> node_map;
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h:92:36: error: ‘enable_if’ in namespace ‘std’ does not name a type
                      typename std::enable_if<std::is_convertible<W*, V*>::value,
                                    ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h:92:45: error: expected ‘,’ or ‘...’ before ‘<’ token
                      typename std::enable_if<std::is_convertible<W*, V*>::value,
                                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h: In member function ‘YAML::detail::node_iterator_value<V>* YAML::detail::node_iterator_base<V>::proxy::operator->()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h:64:51: error: ‘addressof’ is not a member of ‘std’
     node_iterator_value<V>* operator->() { return std::addressof(m_ref); }
                                                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h: In member function ‘YAML::detail::node_iterator_base<V>::proxy::operator YAML::detail::node_iterator_value<V>*()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_iterator.h:65:49: error: ‘addressof’ is not a member of ‘std’
     operator node_iterator_value<V>*() { return std::addressof(m_ref); }
                                                 ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/iterator.h:13:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:11,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:43:41: error: ‘shared_memory_holder’ has not been declared
   explicit iterator_base(base_type rhs, shared_memory_holder pMemory)
                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:48:31: error: ‘enable_if’ in namespace ‘std’ does not name a type
                 typename std::enable_if<std::is_convertible<W*, V*>::value,
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:48:40: error: expected ‘,’ or ‘...’ before ‘<’ token
                 typename std::enable_if<std::is_convertible<W*, V*>::value,
                                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:86:3: error: ‘shared_memory_holder’ does not name a type
   shared_memory_holder m_pMemory;
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: In member function ‘V* YAML::detail::iterator_base<V>::proxy::operator->()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:32:30: error: ‘addressof’ is not a member of ‘std’
     V* operator->() { return std::addressof(m_ref); }
                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: In member function ‘YAML::detail::iterator_base<V>::proxy::operator V*()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:33:28: error: ‘addressof’ is not a member of ‘std’
     operator V*() { return std::addressof(m_ref); }
                            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: In constructor ‘YAML::detail::iterator_base<V>::iterator_base()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:42:35: error: class ‘YAML::detail::iterator_base<V>’ does not have any field named ‘m_pMemory’
   iterator_base() : m_iterator(), m_pMemory() {}
                                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: In constructor ‘YAML::detail::iterator_base<V>::iterator_base(YAML::detail::iterator_base<V>::base_type, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:44:26: error: class ‘YAML::detail::iterator_base<V>’ does not have any field named ‘m_pMemory’
       : m_iterator(rhs), m_pMemory(pMemory) {}
                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: In constructor ‘YAML::detail::iterator_base<V>::iterator_base(const YAML::detail::iterator_base<W>&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:50:37: error: class ‘YAML::detail::iterator_base<V>’ does not have any field named ‘m_pMemory’
       : m_iterator(rhs.m_iterator), m_pMemory(rhs.m_pMemory) {}
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h: In member function ‘YAML::detail::iterator_base<V>::value_type YAML::detail::iterator_base<V>::operator*() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:76:34: error: ‘m_pMemory’ was not declared in this scope
       return value_type(Node(*v, m_pMemory));
                                  ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/iterator.h:78:40: error: ‘m_pMemory’ was not declared in this scope
       return value_type(Node(*v.first, m_pMemory), Node(*v.second, m_pMemory));
                                        ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:12:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:29:20: error: ‘shared_node’ was not declared in this scope
   typedef std::set<shared_node> Nodes;
                    ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:29:31: error: template argument 1 is invalid
   typedef std::set<shared_node> Nodes;
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:29:31: error: template argument 2 is invalid
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:29:31: error: template argument 3 is invalid
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:41:3: error: ‘shared_memory’ does not name a type
   shared_memory m_pMemory;
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h: In constructor ‘YAML::detail::memory_holder::memory_holder()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:35:21: error: class ‘YAML::detail::memory_holder’ does not have any field named ‘m_pMemory’
   memory_holder() : m_pMemory(new memory) {}
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h: In member function ‘YAML::detail::node& YAML::detail::memory_holder::create_node()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/memory.h:37:32: error: ‘m_pMemory’ was not declared in this scope
   node& create_node() { return m_pMemory->create_node(); }
                                ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:13:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:14,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:13,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:33:33: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   node_data(const node_data&) = delete;
                                 ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:34:44: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   node_data& operator=(const node_data&) = delete;
                                            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:63:30: error: ‘shared_memory_holder’ has not been declared
   void push_back(node& node, shared_memory_holder pMemory);
                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:64:39: error: ‘shared_memory_holder’ has not been declared
   void insert(node& key, node& value, shared_memory_holder pMemory);
                                       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:68:29: error: ‘shared_memory_holder’ has not been declared
   node* get(const Key& key, shared_memory_holder pMemory) const;
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:70:29: error: ‘shared_memory_holder’ has not been declared
   node& get(const Key& key, shared_memory_holder pMemory);
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:72:31: error: ‘shared_memory_holder’ has not been declared
   bool remove(const Key& key, shared_memory_holder pMemory);
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:74:24: error: ‘shared_memory_holder’ has not been declared
   node* get(node& key, shared_memory_holder pMemory) const;
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:75:24: error: ‘shared_memory_holder’ has not been declared
   node& get(node& key, shared_memory_holder pMemory);
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:76:26: error: ‘shared_memory_holder’ has not been declared
   bool remove(node& key, shared_memory_holder pMemory);
                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:81:21: error: ‘shared_memory_holder’ has not been declared
                     shared_memory_holder pMemory);
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:94:23: error: ‘shared_memory_holder’ has not been declared
   void convert_to_map(shared_memory_holder pMemory);
                       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:95:32: error: ‘shared_memory_holder’ has not been declared
   void convert_sequence_to_map(shared_memory_holder pMemory);
                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:98:46: error: ‘shared_memory_holder’ has not been declared
   static node& convert_to_node(const T& rhs, shared_memory_holder pMemory);
                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_data.h:117:45: error: ‘>>’ should be ‘> >’ within a nested template argument list
   typedef std::vector<std::pair<node*, node*>> node_map;
                                             ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:14:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:13,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:20:31: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   node_ref(const node_ref&) = delete;
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:21:42: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   node_ref& operator=(const node_ref&) = delete;
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:54:30: error: ‘shared_memory_holder’ has not been declared
   void push_back(node& node, shared_memory_holder pMemory) {
                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:57:39: error: ‘shared_memory_holder’ has not been declared
   void insert(node& key, node& value, shared_memory_holder pMemory) {
                                       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:63:29: error: ‘shared_memory_holder’ has not been declared
   node* get(const Key& key, shared_memory_holder pMemory) const {
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:67:29: error: ‘shared_memory_holder’ has not been declared
   node& get(const Key& key, shared_memory_holder pMemory) {
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:71:31: error: ‘shared_memory_holder’ has not been declared
   bool remove(const Key& key, shared_memory_holder pMemory) {
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:75:24: error: ‘shared_memory_holder’ has not been declared
   node* get(node& key, shared_memory_holder pMemory) const {
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:78:24: error: ‘shared_memory_holder’ has not been declared
   node& get(node& key, shared_memory_holder pMemory) {
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:81:26: error: ‘shared_memory_holder’ has not been declared
   bool remove(node& key, shared_memory_holder pMemory) {
                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:88:21: error: ‘shared_memory_holder’ has not been declared
                     shared_memory_holder pMemory) {
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:93:3: error: ‘shared_node_data’ does not name a type
   shared_node_data m_pData;
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In constructor ‘YAML::detail::node_ref::node_ref()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:19:16: error: class ‘YAML::detail::node_ref’ does not have any field named ‘m_pData’
   node_ref() : m_pData(new node_data) {}
                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘bool YAML::detail::node_ref::is_defined() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:23:36: error: ‘m_pData’ was not declared in this scope
   bool is_defined() const { return m_pData->is_defined(); }
                                    ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘const YAML::Mark& YAML::detail::node_ref::mark() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:24:37: error: ‘m_pData’ was not declared in this scope
   const Mark& mark() const { return m_pData->mark(); }
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::NodeType::value YAML::detail::node_ref::type() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:25:41: error: ‘m_pData’ was not declared in this scope
   NodeType::value type() const { return m_pData->type(); }
                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘const string& YAML::detail::node_ref::scalar() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:26:46: error: ‘m_pData’ was not declared in this scope
   const std::string& scalar() const { return m_pData->scalar(); }
                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘const string& YAML::detail::node_ref::tag() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:27:43: error: ‘m_pData’ was not declared in this scope
   const std::string& tag() const { return m_pData->tag(); }
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::EmitterStyle::value YAML::detail::node_ref::style() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:28:46: error: ‘m_pData’ was not declared in this scope
   EmitterStyle::value style() const { return m_pData->style(); }
                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::mark_defined()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:30:25: error: ‘m_pData’ was not declared in this scope
   void mark_defined() { m_pData->mark_defined(); }
                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_data(const YAML::detail::node_ref&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:31:40: error: ‘m_pData’ was not declared in this scope
   void set_data(const node_ref& rhs) { m_pData = rhs.m_pData; }
                                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:31:54: error: ‘const class YAML::detail::node_ref’ has no member named ‘m_pData’
   void set_data(const node_ref& rhs) { m_pData = rhs.m_pData; }
                                                      ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_mark(const YAML::Mark&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:33:37: error: ‘m_pData’ was not declared in this scope
   void set_mark(const Mark& mark) { m_pData->set_mark(mark); }
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_type(YAML::NodeType::value)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:34:41: error: ‘m_pData’ was not declared in this scope
   void set_type(NodeType::value type) { m_pData->set_type(type); }
                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_tag(const string&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:35:42: error: ‘m_pData’ was not declared in this scope
   void set_tag(const std::string& tag) { m_pData->set_tag(tag); }
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_null()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:36:21: error: ‘m_pData’ was not declared in this scope
   void set_null() { m_pData->set_null(); }
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_scalar(const string&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:37:48: error: ‘m_pData’ was not declared in this scope
   void set_scalar(const std::string& scalar) { m_pData->set_scalar(scalar); }
                                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::set_style(YAML::EmitterStyle::value)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:38:47: error: ‘m_pData’ was not declared in this scope
   void set_style(EmitterStyle::value style) { m_pData->set_style(style); }
                                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘std::size_t YAML::detail::node_ref::size() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:41:37: error: ‘m_pData’ was not declared in this scope
   std::size_t size() const { return m_pData->size(); }
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::const_node_iterator YAML::detail::node_ref::begin() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:44:43: error: ‘m_pData’ was not declared in this scope
     return static_cast<const node_data&>(*m_pData).begin();
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::node_iterator YAML::detail::node_ref::begin()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:46:34: error: ‘m_pData’ was not declared in this scope
   node_iterator begin() { return m_pData->begin(); }
                                  ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::const_node_iterator YAML::detail::node_ref::end() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:49:43: error: ‘m_pData’ was not declared in this scope
     return static_cast<const node_data&>(*m_pData).end();
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::node_iterator YAML::detail::node_ref::end()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:51:32: error: ‘m_pData’ was not declared in this scope
   node_iterator end() { return m_pData->end(); }
                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::push_back(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:55:5: error: ‘m_pData’ was not declared in this scope
     m_pData->push_back(node, pMemory);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::insert(YAML::detail::node&, YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:58:5: error: ‘m_pData’ was not declared in this scope
     m_pData->insert(key, value, pMemory);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::node* YAML::detail::node_ref::get(const Key&, int) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:64:43: error: ‘m_pData’ was not declared in this scope
     return static_cast<const node_data&>(*m_pData).get(key, pMemory);
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::node& YAML::detail::node_ref::get(const Key&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:68:12: error: ‘m_pData’ was not declared in this scope
     return m_pData->get(key, pMemory);
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘bool YAML::detail::node_ref::remove(const Key&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:72:12: error: ‘m_pData’ was not declared in this scope
     return m_pData->remove(key, pMemory);
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::node* YAML::detail::node_ref::get(YAML::detail::node&, int) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:76:43: error: ‘m_pData’ was not declared in this scope
     return static_cast<const node_data&>(*m_pData).get(key, pMemory);
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘YAML::detail::node& YAML::detail::node_ref::get(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:79:12: error: ‘m_pData’ was not declared in this scope
     return m_pData->get(key, pMemory);
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘bool YAML::detail::node_ref::remove(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:82:12: error: ‘m_pData’ was not declared in this scope
     return m_pData->remove(key, pMemory);
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h: In member function ‘void YAML::detail::node_ref::force_insert(const Key&, const Value&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node_ref.h:89:5: error: ‘m_pData’ was not declared in this scope
     m_pData->force_insert(key, value, pMemory);
     ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:13:0,
                 from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:22:23: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   node(const node&) = delete;
                       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:23:34: warning: defaulted and deleted functions only available with -std=c++11 or -std=gnu++11 [enabled by default]
   node& operator=(const node&) = delete;
                                  ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:37:29: error: ‘shared_memory_holder’ has not been declared
   bool equals(const T& rhs, shared_memory_holder pMemory);
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:38:32: error: ‘shared_memory_holder’ has not been declared
   bool equals(const char* rhs, shared_memory_holder pMemory);
                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:109:30: error: ‘shared_memory_holder’ has not been declared
   void push_back(node& node, shared_memory_holder pMemory) {
                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:113:39: error: ‘shared_memory_holder’ has not been declared
   void insert(node& key, node& value, shared_memory_holder pMemory) {
                                       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:121:29: error: ‘shared_memory_holder’ has not been declared
   node* get(const Key& key, shared_memory_holder pMemory) const {
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:128:29: error: ‘shared_memory_holder’ has not been declared
   node& get(const Key& key, shared_memory_holder pMemory) {
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:134:31: error: ‘shared_memory_holder’ has not been declared
   bool remove(const Key& key, shared_memory_holder pMemory) {
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:138:24: error: ‘shared_memory_holder’ has not been declared
   node* get(node& key, shared_memory_holder pMemory) const {
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:144:24: error: ‘shared_memory_holder’ has not been declared
   node& get(node& key, shared_memory_holder pMemory) {
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:150:26: error: ‘shared_memory_holder’ has not been declared
   bool remove(node& key, shared_memory_holder pMemory) {
                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:157:21: error: ‘shared_memory_holder’ has not been declared
                     shared_memory_holder pMemory) {
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:162:3: error: ‘shared_node_ref’ does not name a type
   shared_node_ref m_pRef;
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In constructor ‘YAML::detail::node::node()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:21:12: error: class ‘YAML::detail::node’ does not have any field named ‘m_pRef’
   node() : m_pRef(new node_ref) {}
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘bool YAML::detail::node::is(const YAML::detail::node&) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:25:43: error: ‘m_pRef’ was not declared in this scope
   bool is(const node& rhs) const { return m_pRef == rhs.m_pRef; }
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:25:57: error: ‘const class YAML::detail::node’ has no member named ‘m_pRef’
   bool is(const node& rhs) const { return m_pRef == rhs.m_pRef; }
                                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘const YAML::detail::node_ref* YAML::detail::node::ref() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:26:40: error: ‘m_pRef’ was not declared in this scope
   const node_ref* ref() const { return m_pRef.get(); }
                                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘bool YAML::detail::node::is_defined() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:28:36: error: ‘m_pRef’ was not declared in this scope
   bool is_defined() const { return m_pRef->is_defined(); }
                                    ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘const YAML::Mark& YAML::detail::node::mark() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:29:37: error: ‘m_pRef’ was not declared in this scope
   const Mark& mark() const { return m_pRef->mark(); }
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::NodeType::value YAML::detail::node::type() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:30:41: error: ‘m_pRef’ was not declared in this scope
   NodeType::value type() const { return m_pRef->type(); }
                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘const string& YAML::detail::node::scalar() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:32:46: error: ‘m_pRef’ was not declared in this scope
   const std::string& scalar() const { return m_pRef->scalar(); }
                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘const string& YAML::detail::node::tag() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:33:43: error: ‘m_pRef’ was not declared in this scope
   const std::string& tag() const { return m_pRef->tag(); }
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::EmitterStyle::value YAML::detail::node::style() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:34:46: error: ‘m_pRef’ was not declared in this scope
   EmitterStyle::value style() const { return m_pRef->style(); }
                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::mark_defined()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:44:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->mark_defined();
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_ref(const YAML::detail::node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:61:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef = rhs.m_pRef;
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:61:18: error: ‘const class YAML::detail::node’ has no member named ‘m_pRef’
     m_pRef = rhs.m_pRef;
                  ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_data(const YAML::detail::node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:66:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->set_data(*rhs.m_pRef);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:66:27: error: ‘const class YAML::detail::node’ has no member named ‘m_pRef’
     m_pRef->set_data(*rhs.m_pRef);
                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_mark(const YAML::Mark&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:69:37: error: ‘m_pRef’ was not declared in this scope
   void set_mark(const Mark& mark) { m_pRef->set_mark(mark); }
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_type(YAML::NodeType::value)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:74:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->set_type(type);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_null()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:78:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->set_null();
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_scalar(const string&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:82:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->set_scalar(scalar);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_tag(const string&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:86:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->set_tag(tag);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::set_style(YAML::EmitterStyle::value)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:92:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->set_style(style);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘std::size_t YAML::detail::node::size() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:96:37: error: ‘m_pRef’ was not declared in this scope
   std::size_t size() const { return m_pRef->size(); }
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::const_node_iterator YAML::detail::node::begin() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:99:42: error: ‘m_pRef’ was not declared in this scope
     return static_cast<const node_ref&>(*m_pRef).begin();
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::node_iterator YAML::detail::node::begin()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:101:34: error: ‘m_pRef’ was not declared in this scope
   node_iterator begin() { return m_pRef->begin(); }
                                  ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::const_node_iterator YAML::detail::node::end() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:104:42: error: ‘m_pRef’ was not declared in this scope
     return static_cast<const node_ref&>(*m_pRef).end();
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::node_iterator YAML::detail::node::end()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:106:32: error: ‘m_pRef’ was not declared in this scope
   node_iterator end() { return m_pRef->end(); }
                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::push_back(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:110:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->push_back(node, pMemory);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::insert(YAML::detail::node&, YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:114:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->insert(key, value, pMemory);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::node* YAML::detail::node::get(const Key&, int) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:125:42: error: ‘m_pRef’ was not declared in this scope
     return static_cast<const node_ref&>(*m_pRef).get(key, pMemory);
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::node& YAML::detail::node::get(const Key&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:129:19: error: ‘m_pRef’ was not declared in this scope
     node& value = m_pRef->get(key, pMemory);
                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘bool YAML::detail::node::remove(const Key&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:135:12: error: ‘m_pRef’ was not declared in this scope
     return m_pRef->remove(key, pMemory);
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::node* YAML::detail::node::get(YAML::detail::node&, int) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:142:42: error: ‘m_pRef’ was not declared in this scope
     return static_cast<const node_ref&>(*m_pRef).get(key, pMemory);
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘YAML::detail::node& YAML::detail::node::get(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:145:19: error: ‘m_pRef’ was not declared in this scope
     node& value = m_pRef->get(key, pMemory);
                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘bool YAML::detail::node::remove(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:151:12: error: ‘m_pRef’ was not declared in this scope
     return m_pRef->remove(key, pMemory);
            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h: In member function ‘void YAML::detail::node::force_insert(const Key&, const Value&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/node.h:158:5: error: ‘m_pRef’ was not declared in this scope
     m_pRef->force_insert(key, value, pMemory);
     ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:17:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In constructor ‘YAML::Node::Node(YAML::NodeType::value)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:22:7: error: class ‘YAML::Node’ does not have any field named ‘m_pMemory’
       m_pMemory(new detail::memory_holder),
       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:23:16: error: ‘m_pMemory’ was not declared in this scope
       m_pNode(&m_pMemory->create_node()) {
                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In constructor ‘YAML::Node::Node(const T&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:30:7: error: class ‘YAML::Node’ does not have any field named ‘m_pMemory’
       m_pMemory(new detail::memory_holder),
       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:31:16: error: ‘m_pMemory’ was not declared in this scope
       m_pNode(&m_pMemory->create_node()) {
                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In constructor ‘YAML::Node::Node(const YAML::detail::iterator_value&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:37:7: error: class ‘YAML::Node’ does not have any field named ‘m_pMemory’
       m_pMemory(rhs.m_pMemory),
       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:37:21: error: ‘const struct YAML::detail::iterator_value’ has no member named ‘m_pMemory’
       m_pMemory(rhs.m_pMemory),
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In copy constructor ‘YAML::Node::Node(const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:42:7: error: class ‘YAML::Node’ does not have any field named ‘m_pMemory’
       m_pMemory(rhs.m_pMemory),
       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:42:21: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
       m_pMemory(rhs.m_pMemory),
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:47:47: error: ‘YAML::detail::shared_memory_holder’ has not been declared
 inline Node::Node(detail::node& node, detail::shared_memory_holder pMemory)
                                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In constructor ‘YAML::Node::Node(YAML::detail::node&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:48:24: error: class ‘YAML::Node’ does not have any field named ‘m_pMemory’
     : m_isValid(true), m_pMemory(pMemory), m_pNode(&node) {}
                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘void YAML::Node::EnsureNodeExists() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:56:5: error: ‘m_pMemory’ was not declared in this scope
     m_pMemory.reset(new detail::memory_holder);
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘void YAML::Node::reset(const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:208:3: error: ‘m_pMemory’ was not declared in this scope
   m_pMemory = rhs.m_pMemory;
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:208:19: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
   m_pMemory = rhs.m_pMemory;
                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘void YAML::Node::AssignData(const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:257:3: error: ‘m_pMemory’ was not declared in this scope
   m_pMemory->merge(*rhs.m_pMemory);
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:257:25: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
   m_pMemory->merge(*rhs.m_pMemory);
                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘void YAML::Node::AssignNode(const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:267:5: error: ‘m_pMemory’ was not declared in this scope
     m_pMemory = rhs.m_pMemory;
     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:267:21: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
     m_pMemory = rhs.m_pMemory;
                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:272:3: error: ‘m_pMemory’ was not declared in this scope
   m_pMemory->merge(*rhs.m_pMemory);
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:272:25: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
   m_pMemory->merge(*rhs.m_pMemory);
                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘YAML::const_iterator YAML::Node::begin() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:286:53: error: ‘m_pMemory’ was not declared in this scope
   return m_pNode ? const_iterator(m_pNode->begin(), m_pMemory)
                                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘YAML::iterator YAML::Node::begin()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:293:47: error: ‘m_pMemory’ was not declared in this scope
   return m_pNode ? iterator(m_pNode->begin(), m_pMemory) : iterator();
                                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘YAML::const_iterator YAML::Node::end() const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:299:51: error: ‘m_pMemory’ was not declared in this scope
   return m_pNode ? const_iterator(m_pNode->end(), m_pMemory) : const_iterator();
                                                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘YAML::iterator YAML::Node::end()’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:305:45: error: ‘m_pMemory’ was not declared in this scope
   return m_pNode ? iterator(m_pNode->end(), m_pMemory) : iterator();
                                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘void YAML::Node::push_back(const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:322:36: error: ‘m_pMemory’ was not declared in this scope
   m_pNode->push_back(*rhs.m_pNode, m_pMemory);
                                    ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:323:25: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
   m_pMemory->merge(*rhs.m_pMemory);
                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘const YAML::Node YAML::Node::operator[](const Key&) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:378:57: error: ‘m_pMemory’ was not declared in this scope
                             .get(detail::to_value(key), m_pMemory);
                                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘YAML::Node YAML::Node::operator[](const Key&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:390:61: error: ‘m_pMemory’ was not declared in this scope
   detail::node& value = m_pNode->get(detail::to_value(key), m_pMemory);
                                                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘bool YAML::Node::remove(const Key&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:399:49: error: ‘m_pMemory’ was not declared in this scope
   return m_pNode->remove(detail::to_value(key), m_pMemory);
                                                 ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘const YAML::Node YAML::Node::operator[](const YAML::Node&) const’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:407:3: error: ‘m_pMemory’ was not declared in this scope
   m_pMemory->merge(*key.m_pMemory);
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:407:25: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
   m_pMemory->merge(*key.m_pMemory);
                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘YAML::Node YAML::Node::operator[](const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:421:3: error: ‘m_pMemory’ was not declared in this scope
   m_pMemory->merge(*key.m_pMemory);
   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:421:25: error: ‘const class YAML::Node’ has no member named ‘m_pMemory’
   m_pMemory->merge(*key.m_pMemory);
                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘bool YAML::Node::remove(const YAML::Node&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:431:40: error: ‘m_pMemory’ was not declared in this scope
   return m_pNode->remove(*key.m_pNode, m_pMemory);
                                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h: In member function ‘void YAML::Node::force_insert(const Key&, const Value&)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/impl.h:441:25: error: ‘m_pMemory’ was not declared in this scope
                         m_pMemory);
                         ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:18:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/convert.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/convert.h:247:16: error: ‘array’ is not a member of ‘std’
 struct convert<std::array<T, N>> {
                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/convert.h:247:16: error: ‘array’ is not a member of ‘std’
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/convert.h:247:31: error: spurious ‘>>’, use ‘>’ to terminate a template argument list
 struct convert<std::array<T, N>> {
                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/convert.h:247:31: error: wrong number of template arguments (2, should be 1)
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:16:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/node.h:142:8: error: provided for ‘template<class T> struct YAML::convert’
 struct convert;
        ^
In file included from ../Source/dependencies/yaml-cpp/include/yaml-cpp/yaml.h:20:0,
                 from ../Source/FAST_cInterface.h:11,
                 from ../Source/FAST_cInterface.c:1:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:19:42: error: ‘shared_memory_holder’ has not been declared
                    const Key& /* key */, shared_memory_holder /* pMemory */) {
                                          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:26:30: error: ‘enable_if’ in namespace ‘std’ does not name a type
                typename std::enable_if<std::is_unsigned<Key>::value &&
                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:26:39: error: expected template-argument before ‘<’ token
                typename std::enable_if<std::is_unsigned<Key>::value &&
                                       ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:26:39: error: expected ‘>’ before ‘<’ token
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:27:78: error: template argument 2 is invalid
                                        !std::is_same<Key, bool>::value>::type> {
                                                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:27:80: error: expected ‘::’ before ‘{’ token
                                        !std::is_same<Key, bool>::value>::type> {
                                                                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:27:80: error: expected identifier before ‘{’ token
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:27:80: error: qualified name does not name a class before ‘{’ token
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:35: error: ‘enable_if’ in namespace ‘std’ does not name a type
 struct get_idx<Key, typename std::enable_if<std::is_signed<Key>::value>::type> {
                                   ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:44: error: expected template-argument before ‘<’ token
 struct get_idx<Key, typename std::enable_if<std::is_signed<Key>::value>::type> {
                                            ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:44: error: expected ‘>’ before ‘<’ token
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:78: error: template argument 2 is invalid
 struct get_idx<Key, typename std::enable_if<std::is_signed<Key>::value>::type> {
                                                                              ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:80: error: expected ‘::’ before ‘{’ token
 struct get_idx<Key, typename std::enable_if<std::is_signed<Key>::value>::type> {
                                                                                ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:80: error: expected identifier before ‘{’ token
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:44:80: error: qualified name does not name a class before ‘{’ token
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:60:40: error: ‘shared_memory_holder’ has not been declared
 inline bool node::equals(const T& rhs, shared_memory_holder pMemory) {
                                        ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:68:43: error: ‘shared_memory_holder’ has not been declared
 inline bool node::equals(const char* rhs, shared_memory_holder pMemory) {
                                           ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:75:29: error: ‘shared_memory_holder’ has not been declared
                             shared_memory_holder pMemory) const {
                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:100:45: error: ‘shared_memory_holder’ has not been declared
 inline node& node_data::get(const Key& key, shared_memory_holder pMemory) {
                                             ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h: In member function ‘YAML::detail::node& YAML::detail::node_data::get(const Key&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:125:20: error: base operand of ‘->’ is not a pointer
   node& v = pMemory->create_node();
                    ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h: At global scope:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:131:47: error: ‘shared_memory_holder’ has not been declared
 inline bool node_data::remove(const Key& key, shared_memory_holder pMemory) {
                                               ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:148:37: error: ‘shared_memory_holder’ has not been declared
                                     shared_memory_holder pMemory) {
                                     ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:168:41: error: ‘shared_memory_holder’ has not been declared
                                         shared_memory_holder pMemory) {
                                         ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h: In static member function ‘static YAML::detail::node& YAML::detail::node_data::convert_to_node(const T&, int)’:
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:171:10: error: base operand of ‘->’ is not a pointer
   pMemory->merge(*value.m_pMemory);
          ^
../Source/dependencies/yaml-cpp/include/yaml-cpp/node/detail/impl.h:171:25: error: ‘class YAML::Node’ has no member named ‘m_pMemory’
   pMemory->merge(*value.m_pMemory);
                         ^
../Source/FAST_cInterface.c: In member function ‘int FAST_cInterface::init()’:
../Source/FAST_cInterface.c:20:61: warning: invalid conversion from ‘void*’ to ‘OpFM_InputType_t* {aka OpFM_InputType*}’ [-fpermissive]
    cDriver_Input_from_FAST = malloc(sizeof(OpFM_InputType_t));
                                                             ^
../Source/FAST_cInterface.c:21:61: warning: invalid conversion from ‘void*’ to ‘OpFM_OutputType_t* {aka OpFM_OutputType*}’ [-fpermissive]
    cDriver_Output_to_FAST = malloc(sizeof(OpFM_OutputType_t));
                                                             ^
make: *** [Obj_lin64/FAST_cInterface.o] Error 1
